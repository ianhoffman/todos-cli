{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import qualified Todo as Todo
import qualified Data.Time.Clock.POSIX as Posix
import           Data.Sort
import           Data.String
import           Database.SQLite.Simple
-- import           Database.SQLite.Simple(close, execute, execute_, open, query_)
-- import           Database.SQLite.Simple.Internal(Connection)
import           System.Environment
import           Text.Read


run :: IO ()
run = getArgs >>= parse


parse :: (Eq a, Data.String.IsString a) => [a] -> IO ()
parse ["-a"] = createTodo
parse ["-e"] = setInProgress
parse ["-l"] = listTodos
parse _ = putStrLn "NotImplemented"


createTodo :: IO ()
createTodo = do
    putStrLn "Add a todo: "
    description <- getLine
    created_at <- fmap round Posix.getPOSIXTime
    insertTodo (Todo.makeTodo description created_at)


setInProgress :: IO ()
setInProgress = setStatus Todo.NotStarted Todo.InProgress


-- setComplete :: IO ()
-- setComplete = setStatus Todo.InProgress Todo.Done


setStatus :: Todo.TodoStatus -> Todo.TodoStatus -> IO ()
setStatus currentStatus desiredStatus = do
    putStrLn "Enter a todo id: "
    userInput <- getLine
    numUpdated <- case parseInt userInput of
        Nothing -> return 0
        Just todoId -> do 
            conn <- getConnection
            executeNamed conn "UPDATE todos SET status=:desired_status WHERE id=:id AND status=:current_status" [
                ":desired_status" := show desiredStatus,
                ":current_status" := show currentStatus,
                ":id" := todoId ]
            close conn
            changes conn
    if numUpdated > 0 
        then putStrLn $ "Updated todo " ++ userInput
        else putStrLn $ userInput ++ " is not a valid id"


-- editTodo :: IO ()
-- editTodo = do 
--     putStrLn "Enter a todo id: "
--     userInput <- getLine
--     numUpdated <- case parseInt userInput of
--         Nothing -> return 0
--         Just todoId -> updateTodoDescription todoId "foobaz"
--     if numUpdated > 0 
--         then putStrLn $ "Updated todo " ++ userInput
--         else putStrLn $ userInput ++ " is not a valid id"


parseInt :: String -> Maybe Int
parseInt = readMaybe


listTodos :: IO ()
listTodos = do
    rows <- getAllTodos
    putStrLn (formatTodos (sort rows))


-- updateTodoDescription :: Int -> String -> IO (Int)
-- updateTodoDescription todoId description = do
--     conn <- getConnection
--     executeNamed conn "UPDATE todos SET description=:description WHERE id=:id" [
--         ":description" := description, 
--         ":id" := todoId ]
--     close conn
--     changes conn


getAllTodos :: IO [Todo.Todo]
getAllTodos = do
    conn <- getConnection
    todos <- query_ conn "SELECT * FROM todos" :: IO [Todo.Todo]
    close conn
    return todos


-- getTodoById :: [Todo.Todo] -> Maybe Int -> Maybe Todo.Todo
-- getTodoById allTodos todoId = case [ t | t <- allTodos, Todo.id_ t == todoId ] of
--     [] -> Nothing
--     (todo:_) -> Just todo


formatTodos :: [Todo.Todo] -> String
formatTodos = Prelude.foldl (\txt todo -> txt ++ formatTodo todo) ""


formatTodo :: Todo.Todo -> String
formatTodo todo = "\
    \=======================================\n\
    \Description: " ++ Todo.description todo ++ "\n\
    \Status: " ++ show (Todo.status todo) ++ "\n\
    \Created: " ++ show (Todo.createdAt todo) ++ "\n\
    \id: " ++ maybe "" show (Todo.id_ todo) ++ "\n"


insertTodo :: Todo.Todo -> IO ()
insertTodo todo = do
    conn <- getConnection
    execute conn "INSERT INTO todos (description, status, created_at) VALUES (?, ?, ?)" todo
    close conn


getConnection :: IO Connection
getConnection = do
    conn <- open ".todos.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS todos (\
        \ id INTEGER PRIMARY KEY,\
        \ description VARCHAR (255) NOT NULL,\
        \ status VARCHAR (255) NOT NULL,\
        \ created_at INTEGER NOT NULL\
        \ )"
    return conn 
