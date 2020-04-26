module Todo where

import Data.Text(unpack)
import qualified Database.SQLite3 as Base
import Database.SQLite.Simple(ResultError(..), toRow, ToRow)
import Database.SQLite.Simple.FromField(fromField, FromField, returnError)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok


data TodoStatus 
    = Done 
    | InProgress 
    | NotStarted 
    deriving (Enum)


instance Show TodoStatus where
    show Done = "Done"
    show InProgress = "In Progress"
    show NotStarted = "Not Started"


instance FromField TodoStatus where
    fromField f = case f of
        (Field (Base.SQLText txt) _) -> case unpack txt of
            "Done" -> Ok(Done)
            "In Progress" -> Ok(InProgress)
            "Not Started" -> Ok(NotStarted)
            _ -> returnError ConversionFailed f "invalid value"
        _ -> returnError ConversionFailed f "need a text"


data Todo = Todo {
    id_ :: Maybe Int,
    description :: String,
    status :: TodoStatus,
    createdAt :: Int
} deriving (Show)


instance Eq Todo where
    (==) t1 t2 = (==) (id_ t1) (id_ t2)


instance Ord Todo where
    -- TODO: Add priority --
    compare t1 t2 = compare (createdAt t2) (createdAt t1)


instance FromRow Todo where
    fromRow = Todo <$> field <*> field <*> field <*> field


instance ToRow Todo where
    toRow todo = toRow (description todo, show (status todo), (createdAt todo) :: Int)


makeTodo :: String -> Int -> Todo
makeTodo d t = Todo Nothing d NotStarted t