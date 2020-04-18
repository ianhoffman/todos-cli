{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.Sort
import Database.SQLite.Simple
import Options.Applicative
import qualified Todo as Todo
import qualified Data.Time.Clock.POSIX as Posix


run :: IO ()
run = do
    options <- execParser opts
    case options of
        Create -> createTodo
        List -> listTodos
        Edit todoId -> updateTodoDescription todoId "testing!"


-- Parser

data Action 
    = Create
    | List 
    | Edit Int


create :: Parser Action
create = flag' Create
    (  long "create"
    <> short 'c'
    <> help "Create a todo" )


list :: Parser Action
list = flag' List
    (  long "list" 
    <> short 'l'
    <> help "List all todos" )


-- TODO: Subparser for descriptions / setting status / changing priority. What is this API? 
edit :: Parser Action
edit = Edit <$> option auto
    (  long "edit"
    <> short 'e' 
    <> help "Edit a todo"
    <> metavar "INT" )


input :: Parser Action
input = create <|> list <|> edit


opts :: ParserInfo Action
opts = info (input <**> helper)
    (  fullDesc 
    <> progDesc "Manage your toods" 
    <> header "This is a test" )


-- Create

createTodo :: IO ()
createTodo = do
    putStrLn "Add a todo: "
    description <- getLine
    createdAt <- round <$> Posix.getPOSIXTime
    insertTodo (Todo.makeTodo description createdAt)


insertTodo :: Todo.Todo -> IO ()
insertTodo todo = getConnection >>= createTable >>= executeInsert todo >>= close


executeInsert :: Todo.Todo -> Connection -> IO Connection
executeInsert todo conn = do
    execute conn "INSERT INTO todos (description, status, created_at) VALUES (?, ?, ?)" todo
    return conn


-- List

listTodos :: IO ()
listTodos = putStrLn =<< formatTodos <$> sort <$> getAllTodos


getAllTodos :: IO [Todo.Todo]
getAllTodos = do
    conn <- getConnection
    todos <- query_ conn "SELECT * FROM todos" :: IO [Todo.Todo]
    close conn
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

updateTodoDescription :: Int -> String -> IO ()
updateTodoDescription todoId description = do
    conn <- getConnection
    executeNamed conn "UPDATE todos SET description=:description WHERE id=:id" [
        ":description" := description, 
        ":id" := todoId ]
    close conn
    numUpdated <- changes conn
    if numUpdated > 0 
        then putStrLn $ "Updated todo " ++ show todoId
        else putStrLn $ show todoId ++ " is not a valid id"


-- Util


getConnection :: IO Connection
getConnection = open ".todos.db"


createTable :: Connection -> IO Connection
createTable conn = do 
    execute_ conn "\
        \CREATE TABLE IF NOT EXISTS todos (\
        \ id INTEGER PRIMARY KEY,\
        \ description VARCHAR (255) NOT NULL,\
        \ status VARCHAR (255) NOT NULL,\
        \ created_at INTEGER NOT NULL\
        \ )"
    return conn


-- setInProgress :: IO ()
-- setInProgress = setStatus Todo.NotStarted Todo.InProgress


-- setComplete :: IO ()
-- setComplete = setStatus Todo.InProgress Todo.Done


-- setStatus :: Todo.TodoStatus -> Todo.TodoStatus -> IO ()
-- setStatus currentStatus desiredStatus = do
--     putStrLn "Enter a todo id: "
--     userInput <- getLine
--     numUpdated <- case parseInt userInput of
--         Nothing -> return 0
--         Just todoId -> do 
--             conn <- getConnection
--             executeNamed conn "UPDATE todos SET status=:desired_status WHERE id=:id AND status=:current_status" [
--                 ":desired_status" := show desiredStatus,
--                 ":current_status" := show currentStatus,
--                 ":id" := todoId ]
--             close conn
--             changes conn
--     if numUpdated > 0 
--         then putStrLn $ "Updated todo " ++ userInput
--         else putStrLn $ userInput ++ " is not a valid id"


-- -- editTodo :: IO ()
-- -- editTodo = do 
-- --     putStrLn "Enter a todo id: "
-- --     userInput <- getLine
-- --     numUpdated <- case parseInt userInput of
-- --         Nothing -> return 0
-- --         Just todoId -> updateTodoDescription todoId "foobaz"
-- --     if numUpdated > 0 
-- --         then putStrLn $ "Updated todo " ++ userInput
-- --         else putStrLn $ userInput ++ " is not a valid id"
