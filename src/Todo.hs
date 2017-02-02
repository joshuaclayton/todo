{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Todo
    ( Msg(..)
    , Todo(..)
    , TodoId(..)
    , DueDate(..)
    , TodoPriority(..)

    , update
    , createNewTodo
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)

data TodoStatus = Complete | Incomplete
data TodoPriority = Low | Medium | High deriving Show
newtype DueDate = DueDate UTCTime
newtype TodoId = TodoId Integer deriving (Enum, Eq, Ord)

data Todo = Todo
    { tId :: TodoId
    , tTitle :: Text
    , tStatus :: TodoStatus
    , tDueDate :: Maybe DueDate
    , tPriority :: TodoPriority
    }

data Msg
    = MarkComplete
    | MarkIncomplete
    | UpdateDueDate DueDate
    | DeleteDueDate
    | IncreasePriority
    | DecreasePriority
    | SetPriority TodoPriority

createNewTodo :: TodoId -> Text -> Todo
createNewTodo id' t = Todo id' t Incomplete Nothing Low

update :: Msg -> Todo -> Todo
update MarkComplete t = t { tStatus = Complete }
update MarkIncomplete t = t { tStatus = Incomplete }
update (UpdateDueDate d) t = t { tDueDate = Just d }
update DeleteDueDate t = t { tDueDate = Nothing }
update IncreasePriority t = t { tPriority = increasePriority $ tPriority t }
update DecreasePriority t = t { tPriority = decreasePriority $ tPriority t }
update (SetPriority p) t = t { tPriority = p }

increasePriority :: TodoPriority -> TodoPriority
increasePriority Low = Medium
increasePriority Medium = High
increasePriority High = High

decreasePriority :: TodoPriority -> TodoPriority
decreasePriority Low = Low
decreasePriority Medium = Low
decreasePriority High = Medium
