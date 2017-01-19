{-# LANGUAGE TemplateHaskell #-}

module TodoList
    ( Msg(..)
    , TodoList(..)

    , createNewTodoList
    , update
    ) where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Todo
import           Todo hiding (Msg, update)

data TodoList = TodoList
    { _tlTitle :: Text
    , _tlTodos :: Map.Map TodoId Todo
    , _tlStartingTodoId :: TodoId
    }

makeLenses ''TodoList

data Msg
    = UpdateTodo Todo.Msg TodoId
    | CreateTodo Text
    | DeleteTodo TodoId
    | UpdateTitle Text

createNewTodoList :: Text -> TodoList
createNewTodoList t = TodoList t Map.empty (TodoId 0)

update :: Msg -> TodoList -> TodoList
update (UpdateTodo msg id') l =
    case findTodo l id' of
        Nothing -> l
        Just t -> l & tlTodos %~ Map.adjust (const $ Todo.update msg t) id'
update (CreateTodo title) l =
    l & tlTodos %~ uncurry Map.insert (generateNewTodo l title)
update (UpdateTitle t) l =
    l & tlTitle .~ t
update (DeleteTodo id') l =
    l & tlTodos %~ Map.delete id'

findTodo :: TodoList -> TodoId -> Maybe Todo
findTodo l id' = Map.lookup id' $ l ^. tlTodos

generateNewTodo :: TodoList -> Text -> (TodoId, Todo)
generateNewTodo l title = (nextId, newTodo)
  where
    newTodo = Todo.createNewTodo nextId title
    nextId = succ $ l ^. tlStartingTodoId
