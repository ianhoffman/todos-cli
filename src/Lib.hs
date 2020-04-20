{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.List(intersperse)
import Data.Sort
import Text.Read (readEither)
import System.Environment
import qualified Data.Char as Char
import qualified Data.Time.Clock.POSIX as Posix
import qualified Database.SQLite.Simple as SQLite
import qualified Options.Applicative as O
import qualified Todo as Todo


run :: IO ()
run = do
    args <- getNewArgs <$> getArgs
    let options = O.execParserPure O.defaultPrefs (O.info input mempty) args
    case options of
        O.Success Create -> createTodo
        O.Success List -> listTodos
        O.Success (Edit todoId field value) -> editTodo todoId field value
        -- Success s -> putStrLn $ "Unexpected success " ++ show s
        O.Failure _ -> putStrLn =<< show <$> O.handleParseResult options
        s -> putStrLn $ "Unexpected error " ++ show s


-- TODO: Figure out how to inline this
getNewArgs :: [String] -> [String]
getNewArgs [] = []
getNewArgs [x] = [x]
getNewArgs (x:xs) = [x] ++ [unwords xs]


-- Parser

data Action 
    = Create
    | List 
    | Edit Int ValidField String
    deriving Show


create :: O.Parser Action
create = O.flag' Create
    $  O.long "create"
    <> O.short 'c'
    <> O.help "Create a todo" 


list :: O.Parser Action
list = O.flag' List
    $  O.long "list" 
    <> O.short 'l'
    <> O.help "List all todos" 


edit :: O.Parser Action
edit = O.option (O.eitherReader $ \inp -> case checkEdit (words inp) of 
        Right (x, y, z) -> Right (Edit x y z)
        Left s -> Left s )
    $  O.long "edit" 
    <> O.short 'e' 
    <> O.metavar "Edit" 
    <> O.help "Edit a todo"


input :: O.Parser Action
input = create O.<|> list O.<|> edit


-- TODO: Get help text working again --
-- opts :: ParserInfo Action
-- opts = info (input <**> helper)
--     (  fullDesc 
--     <> progDesc "Manage your todos" 
--     <> header "todo-cli: Manage your todos from the CLI" )


-- Create

-- TODO: Supply arg via argument parser, not getLine --
createTodo :: IO ()
createTodo = do
    putStrLn "Add a todo: "
    description <- getLine
    createdAt <- round <$> Posix.getPOSIXTime
    insertTodo (Todo.makeTodo description createdAt)


insertTodo :: Todo.Todo -> IO ()
insertTodo todo = getConnection >>= ensureTable >>= executeInsert todo >>= SQLite.close


executeInsert :: Todo.Todo -> SQLite.Connection -> IO SQLite.Connection
executeInsert todo conn = do
    SQLite.execute conn "INSERT INTO todos (description, status, created_at) VALUES (?, ?, ?)" todo
    return conn


-- List

listTodos :: IO ()
listTodos = putStrLn =<< formatTodos <$> sort <$> getAllTodos


getAllTodos :: IO [Todo.Todo]
getAllTodos = do
    conn <- ensureTable =<< getConnection
    todos <- SQLite.query_ conn "SELECT * FROM todos" :: IO [Todo.Todo]
    SQLite.close conn
    return todos


formatTodos :: [Todo.Todo] -> String
formatTodos = Prelude.foldl (\txt todo -> txt ++ formatTodo todo) ""


formatTodo :: Todo.Todo -> String
formatTodo todo = "\
    \=======================================\n\
    \Description: " ++ Todo.description todo ++ "\n\
    \Status: " ++ show (Todo.status todo) ++ "\n\
    \Created: " ++ show (Todo.createdAt todo) ++ "\n\
    \id: " ++ (maybe "" show (Todo.id_ todo)) ++ "\n"


-- Update

editTodo :: Int -> ValidField -> String -> IO ()
editTodo todoId field value = do 
    conn <- getConnection
    SQLite.executeNamed 
        conn 
        (case field of
            Description -> "UPDATE todos SET description=:desired_value WHERE id=:id"
            Status -> "UPDATE todos SET status=:desired_value WHERE id=:id") 
        [
            ":desired_value" SQLite.:= value, 
            ":id" SQLite.:= todoId]
    SQLite.close conn
    numUpdated <- SQLite.changes conn
    if numUpdated > 0
        then putStrLn $ "Updated todo " ++ show todoId
        else putStrLn $ show todoId ++ " is not a valid id"


-- Util

capitalize :: String -> String
capitalize (x:xs) = Char.toUpper x : xs
capitalize [] = []


commaConcat :: [String] -> String
commaConcat = concat . intersperse ", " . map (\w -> "\"" ++ w ++ "\"")


getConnection :: IO SQLite.Connection
getConnection = SQLite.open ".todos.db"


ensureTable :: SQLite.Connection -> IO SQLite.Connection
ensureTable conn = do 
    SQLite.execute_ conn "\
        \CREATE TABLE IF NOT EXISTS todos (\
        \ id INTEGER PRIMARY KEY,\
        \ description VARCHAR (255) NOT NULL,\
        \ status VARCHAR (255) NOT NULL,\
        \ created_at INTEGER NOT NULL\
        \ )"
    return conn


-- Validation

data ValidField
    = Description
    | Status
    deriving (Bounded, Enum, Eq, Read, Show)


readField :: String -> Either String ValidField
readField = readEither . capitalize


checkEdit :: [String] -> Either String (Int, ValidField, String)
checkEdit [todoId, field, value] = (,,) 
    <$> checkId todoId
    <*> checkField field
    <*> checkValue field value
checkEdit _ = Left "Edit expects a todo id, a field to edit, and a value for that field"


checkId :: String -> Either String Int
checkId = readEither


checkField :: String -> Either String ValidField
checkField field = case readField field of
    Right validField -> Right validField
    Left _ -> Left $ field
        ++ " is not a valid field. Valid options are " 
        ++ commaConcat (map show [Description, Status])


checkValue :: String -> String -> Either String String
checkValue field value = case readField field of
    Right validField -> case validField of
        Description -> Right value
        Status -> case value `elem` validValues of
            True -> Right value
            False -> Left $ value 
                ++ " must be one of " 
                ++ commaConcat validValues
        where validValues = ["In Progress", "Done", "Not Started"]
    Left s -> Left s
