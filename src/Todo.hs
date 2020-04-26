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
    | WontDo
    deriving (Enum, Eq)


instance Show TodoStatus where
    show Done = "Done"
    show InProgress = "In Progress"
    show NotStarted = "Not Started"
    show WontDo = "Won't Do"


instance Ord TodoStatus where
    compare t1 t2 = compare (todoStatusToInt t1) (todoStatusToInt t2)


todoStatusToInt :: TodoStatus -> Int
todoStatusToInt Done = 3
todoStatusToInt InProgress = 2
todoStatusToInt NotStarted = 1
todoStatusToInt WontDo = 0


instance FromField TodoStatus where
    fromField f = case f of
        (Field (Base.SQLText txt) _) -> case unpack txt of
            "Done" -> Ok(Done)
            "In Progress" -> Ok(InProgress)
            "Not Started" -> Ok(NotStarted)
            _ -> returnError ConversionFailed f "invalid value"
        _ -> returnError ConversionFailed f "need a text"


data TodoPriority 
    = Urgent
    | High
    | Medium
    | Low
    deriving (Show, Eq, Read)


instance Ord TodoPriority where
    compare t1 t2 = compare (todoPriorityToInt t1) (todoPriorityToInt t2)


todoPriorityToInt :: TodoPriority -> Int
todoPriorityToInt Urgent = 3
todoPriorityToInt High = 2
todoPriorityToInt Medium = 1
todoPriorityToInt Low = 0


instance FromField TodoPriority where
    fromField f = case f of 
        (Field (Base.SQLText txt) _) -> case unpack txt of
            "Urgent" -> Ok(Urgent)
            "High" -> Ok(High)
            "Medium" -> Ok(Medium)
            "Low" -> Ok(Low)
            _ -> returnError ConversionFailed f "invalid value"
        _ -> returnError ConversionFailed f "need a text"


data Todo = Todo {
    id_ :: Maybe Int,
    description :: String,
    priority :: TodoPriority,
    status :: TodoStatus,
    createdAt :: Int
} deriving (Show)


instance FromRow Todo where
    fromRow = Todo <$> field <*> field <*> field <*> field <*> field


instance ToRow Todo where
    toRow todo = toRow (
        description todo, 
        show (priority todo), 
        show (status todo), 
        (createdAt todo) :: Int)


makeTodo :: String -> String -> Int -> Todo
makeTodo d p t = Todo Nothing d (read p :: TodoPriority) NotStarted t