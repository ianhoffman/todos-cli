{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.List(intersperse)
import Data.Sort
import Text.Read (readEither)
import qualified Data.Char as Char
import qualified Control.Monad as CM
import qualified Data.Time.Clock.POSIX as Posix
import qualified Database.SQLite.Simple as SQLite
import qualified Options.Applicative as O
import qualified Todo as Todo


-- Entrypoint

run :: IO ()
run = CM.join $ O.execParser (
    O.info 
    (opts O.<**> O.helper)
    (O.fullDesc 
    <> O.progDesc "Manage your todos"
    <> O.header "todo-cli: Mange your todos from the CLI"))


-- Parser

-- TODO: More info for each subcommand
opts :: O.Parser (IO ())
opts = O.subparser (
        O.command "create" (
            O.info (
                subOpts
                $ create 
                <$> O.strArgument (O.metavar "DESCRIPTION"))
            (subDesc "Create a todo")
        )
        <> O.command "edit" (
            O.info (
                subOpts
                $ edit 
                <$> O.argument O.auto (O.metavar "TODO_ID")
                <*> O.strArgument (O.metavar "FIELD") 
                <*> O.strArgument (O.metavar "VALUE"))
            (subDesc "Edit a todo")
        ) 
        <>  O.command "list" (
            O.info (
                subOpts 
                $ pure list) 
            (subDesc "List all todos")
        )
    )
    where subDesc desc = O.fullDesc <> O.progDesc desc <> O.header desc
          subOpts action = action O.<**> O.helper




-- Create

create :: String -> IO ()
create desc = do
    createdAt <- round <$> Posix.getPOSIXTime
    conn <- getConnection
    SQLite.execute 
        conn 
        "INSERT INTO todos (description, status, created_at) VALUES (?, ?, ?)" 
        (Todo.makeTodo desc createdAt)
    SQLite.close conn
    -- TODO: Output something


-- List

list :: IO ()
list = do
    conn <- getConnection
    todos <- SQLite.query_ conn "SELECT * FROM todos" :: IO [Todo.Todo]
    SQLite.close conn
    putStrLn $ Prelude.foldl (\txt todo -> txt ++ formatTodo todo) "" $ sort todos
        where formatTodo t = "\
            \=======================================\n\
            \Description: " ++ Todo.description t ++ "\n\
            \Status: " ++ show (Todo.status t) ++ "\n\
            \Created: " ++ show (Todo.createdAt t) ++ "\n\
            \id: " ++ (maybe "" show (Todo.id_ t)) ++ "\n"


-- Edit

edit :: Int -> String -> String -> IO ()
edit x y z = case checkEdit x y z of  -- TODO: Rename these or find a better way of doing this.
        Right (todoId, field, value) -> do
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
        Left s -> putStrLn $ show s


-- Util

capitalize :: String -> String
capitalize (x:xs) = Char.toUpper x : xs
capitalize [] = []


commaConcat :: [String] -> String
commaConcat = concat . intersperse ", " . map (\w -> "\"" ++ w ++ "\"")


getConnection :: IO SQLite.Connection
getConnection = do
    conn <- SQLite.open ".todos.db"
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


checkEdit :: Int -> String -> String -> Either String (Int, ValidField, String)
checkEdit todoId field value = (,,) 
    <$> Right todoId
    <*> checkField field
    <*> checkValue field value


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
