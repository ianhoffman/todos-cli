{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.List(intersperse)
import Data.Sort
import Data.Time(utctDay)
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
                <$> O.strArgument (O.metavar "DESCRIPTION")
                <*> O.strOption (
                    O.long "priority"
                    <> O.short 'p'
                    <> O.help "Priority of the todo"
                    <> O.value "Medium"
                    <> O.metavar "PRIORITY"))
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
                $ list
                <$> O.switch ( 
                    O.long "verbose" 
                    <> O.short 'v' 
                    <> O.help "Display verbose output")
                <*> O.strOption (
                    O.long "sort"
                    <> O.short 's'
                    <> O.value "priority"
                    <> O.metavar "SORT"
                    <> O.help "Sort options")
                <*> O.switch (
                    O.long "reverse"
                    <> O.short 'r'
                    <> O.help "Sort in reverse")
                <*> O.switch (
                    O.long "all"
                    <> O.short 'a'
                    <> O.help "Show all todos")
                ) 
            (subDesc "List all todos")
        )
    )
    where subDesc desc = O.fullDesc <> O.progDesc desc <> O.header desc
          subOpts action = action O.<**> O.helper




-- Create

create :: String -> String -> IO ()
create desc priority = do
    createdAt <- round <$> Posix.getPOSIXTime
    conn <- getConnection
    case checkValue "priority" priority of 
        Right validPriority -> do
            SQLite.execute 
                conn 
                "INSERT INTO todos (description, priority, status, created_at) VALUES (?, ?, ?, ?)" 
                (Todo.makeTodo desc validPriority createdAt)
            SQLite.close conn
            putStrLn =<< 
                (\r -> "Todo " ++ r ++ " (" ++ desc ++ ") created on " ++ formatCreatedAt createdAt) 
                . show 
                <$> SQLite.lastInsertRowId conn
        Left s -> putStrLn s


-- List

list :: Bool -> String -> Bool -> Bool -> IO ()
list verbose s r a = do
    conn <- getConnection
    todos <- filterTodos a <$> SQLite.query_ conn "SELECT * FROM todos" :: IO [Todo.Todo]
    SQLite.close conn
    putStrLn $ case sortTodos todos s r of 
        Right ts -> Prelude.foldl (\txt todo -> txt ++ formatTodo verbose todo ++ "\n") "" ts
        Left err -> err


-- Edit

edit :: Int -> String -> String -> IO ()
edit x y z = case checkEdit x y z of  -- TODO: Rename these or find a better way of doing this.
        Right (todoId, field, value) -> do
            conn <- getConnection
            SQLite.executeNamed 
                conn 
                (case field of
                    Description -> "UPDATE todos SET description=:desired_value WHERE id=:id"
                    Priority -> "UPDATE todos SET priority=:desired_value WHERE id=:id"
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


filterTodos :: Bool -> [Todo.Todo] -> [Todo.Todo]
filterTodos a todos = case a of
    True -> todos
    False -> filter (\t -> notElem (Todo.status t) [Todo.Done, Todo.WontDo]) todos


formatCreatedAt :: Int -> String
formatCreatedAt = show . utctDay . Posix.posixSecondsToUTCTime . fromIntegral 


formatTodo :: Bool -> Todo.Todo -> String
formatTodo verbose = concat 
    . case verbose of
        True -> intersperse "\n"
            . ("=====================================":)
            . map (\t -> fst t ++ ": " ++ snd t) 
            . zip ["Description", "Priority", "Status", "Created", "ID"] 
        False -> intersperse "|" 
    . ([maybe "" show . Todo.id_, 
        show . Todo.priority,
        show . Todo.status,
        formatCreatedAt . Todo.createdAt,
        Todo.description] <*>)
    . pure


getConnection :: IO SQLite.Connection
getConnection = do
    conn <- SQLite.open ".todos.db"
    SQLite.execute_ conn "\
        \CREATE TABLE IF NOT EXISTS todos (\
        \ id INTEGER PRIMARY KEY,\
        \ description VARCHAR (255) NOT NULL,\
        \ priority VARCHAR (255) NOT NULL,\
        \ status VARCHAR (255) NOT NULL,\
        \ created_at INTEGER NOT NULL\
        \ )"
    return conn


-- Validation

data ValidField
    = Description
    | Priority
    | Status
    deriving (Bounded, Enum, Eq, Read, Show)


readField :: String -> Either String ValidField
readField = readEither . capitalize


checkEdit :: Int -> String -> String -> Either String (Int, ValidField, String)
checkEdit todoId field value = (,,) 
    todoId
    <$> checkField field
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
        Priority -> case value `elem` validValues of
            True -> Right value
            False -> Left $ value 
                ++ " must be one of " 
                ++ commaConcat validValues
            where validValues = ["Urgent", "High", "Medium", "Low"]
        Status -> case value `elem` validValues of
            True -> Right value
            False -> Left $ value 
                ++ " must be one of " 
                ++ commaConcat validValues
            where validValues = ["In Progress", "Done", "Not Started"]
    Left s -> Left s


-- Sorting

data ValidSort
    = PrioritySort
    | StatusSort
    | TimestampSort


readSort :: String -> Either String ValidSort
readSort "priority" = Right PrioritySort
readSort "status" = Right StatusSort
readSort "timestamp" = Right TimestampSort
readSort s = Left $ "Invalid sort option \"" ++ s ++ "\""


sortTodos :: [Todo.Todo] -> String -> Bool -> Either String [Todo.Todo]
sortTodos todos s r = case readSort s of
    Right validSort -> 
        Right $ sortBy (\t1 t2 -> (maybeFlip r . compareTodos) validSort t1 t2) todos
    Left err -> Left $ show err


maybeFlip :: Bool -> (b -> b -> c) -> b -> b -> c
maybeFlip True = flip
maybeFlip False = id


compareTodos :: ValidSort -> Todo.Todo -> Todo.Todo -> Ordering
compareTodos validSort t1 t2 = case validSort of
    PrioritySort -> runCompare [comparePriority, compareStatus, compareCreatedAt] t1 t2
    StatusSort -> runCompare [compareStatus, comparePriority, compareCreatedAt] t1 t2
    TimestampSort -> runCompare [compareCreatedAt, comparePriority, compareStatus] t1 t2


runCompare :: [(Todo.Todo -> Todo.Todo -> Ordering)] -> Todo.Todo -> Todo.Todo -> Ordering
runCompare [] _ _ = EQ
runCompare (comparator:comparators) t1 t2 = case comparator t1 t2 of
    EQ -> runCompare comparators t1 t2
    x -> x


comparePriority :: Todo.Todo -> Todo.Todo -> Ordering
comparePriority t1 t2 = compare (Todo.priority t1) (Todo.priority t2)


compareStatus :: Todo.Todo -> Todo.Todo -> Ordering
compareStatus t1 t2 = compare (Todo.status t1) (Todo.status t2)


compareCreatedAt :: Todo.Todo -> Todo.Todo -> Ordering
compareCreatedAt t1 t2 = compare (Todo.createdAt t1) (Todo.createdAt t2)
