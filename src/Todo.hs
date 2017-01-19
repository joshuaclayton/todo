{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Todo
    ( Msg(..)
    , Todo(..)
    , TodoId(..)
    , DueDate(..)

    , update
    , createNewTodo
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)

data TodoStatus = Complete | Incomplete
newtype DueDate = DueDate UTCTime
newtype TodoId = TodoId Integer deriving (Enum, Eq, Ord)

data Todo = Todo
    { tId :: TodoId
    , tTitle :: Text
    , tStatus :: TodoStatus
    , tDueDate :: Maybe DueDate
    }

data Msg
    = MarkComplete
    | MarkIncomplete
    | UpdateDueDate DueDate
    | DeleteDueDate

createNewTodo :: TodoId -> Text -> Todo
createNewTodo id' t = Todo id' t Incomplete Nothing

update :: Msg -> Todo -> Todo
update MarkComplete t = t { tStatus = Complete }
update MarkIncomplete t = t { tStatus = Incomplete }
update (UpdateDueDate d) t = t { tDueDate = Just d }
update DeleteDueDate t = t { tDueDate = Nothing }
