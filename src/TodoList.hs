module TodoList
    ( Msg(..)
    , TodoList(..)
    , createNewTodoList
    , update
    ) where

import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Todo
import           Todo hiding (Msg, update)

data TodoList = TodoList
    { tlTodos :: Map.Map TodoId Todo
    , tlStartingTodoId :: TodoId
    , tlTitle :: Text
    }

data Msg
    = UpdateTodo Todo.Msg TodoId
    | CreateTodo Text
    | DeleteTodo TodoId

createNewTodoList :: Text -> TodoList
createNewTodoList = TodoList Map.empty (TodoId 0)

update :: Msg -> TodoList -> TodoList
update (UpdateTodo msg id') l =
    case originalTodo of
        Nothing -> l
        Just t -> l { tlTodos = Map.adjust (const $ Todo.update msg t) id' todos }
  where
    todos = tlTodos l
    originalTodo = todos !? id'
update (CreateTodo title) l =
    l { tlTodos = Map.insert todoId newTodo (tlTodos l) }
  where
    newTodo = Todo.createNewTodo todoId title
    todoId = succ $ tlStartingTodoId l
update (DeleteTodo tid) l = l { tlTodos = Map.delete tid (tlTodos l) }
