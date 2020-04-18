{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import qualified Todo as T
import qualified Data.Time.Clock.POSIX as Posix
import           Data.Sort
import           Data.String
import           Database.SQLite.Simple(close, execute, execute_, open, query_)
import           Database.SQLite.Simple.Internal(Connection)
import           System.Environment


run :: IO ()
run = getArgs >>= parse


parse :: (Eq a, Data.String.IsString a) => [a] -> IO ()
parse ["-a"] = createTodo
parse ["-e"] = editTodo
parse ["-l"] = listTodos
parse _ = putStrLn "NotImplemented"


createTodo :: IO ()
createTodo = do
    putStrLn "Add a Todo: "
    description <- getLine
    created_at <- fmap round Posix.getPOSIXTime
    insertTodo (T.makeTodo description created_at)


listTodos :: IO ()
listTodos = do
    conn <- getConnection
    rows <- query_ conn "SELECT * FROM todos" :: IO [T.Todo]
    putStrLn (formatTodos (sort rows))
    close conn


formatTodos :: [T.Todo] -> String
formatTodos = foldl (\txt todo -> txt ++ formatTodo todo) ""


formatTodo :: T.Todo -> String
formatTodo todo = "\
    \=======================================\n\
    \Description: " ++ T.description todo ++ "\n\
    \Status: " ++ show (T.status todo) ++ "\n\
    \Created: " ++ show (T.createdAt todo) ++ "\n\
    \id: " ++ maybe "" show (T.id_ todo) ++ "\n"


insertTodo :: T.Todo -> IO ()
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
