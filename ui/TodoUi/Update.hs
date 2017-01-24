module TodoUi.Update where

import Control.Lens.Operators
import TodoList
import TodoUi.Types
import TodoUi.Util

data Msg
    = NoOp
    | NavigateTo Route
    | UpdateTodoList TodoList.Msg
    | CreateTodoFromModel

update :: TodoUi.Update.Msg -> Model -> Model
update msg model =
    case msg of
        NoOp -> model
        NavigateTo route -> model & mRoute .~ route
        UpdateTodoList msg' -> model & mTodoList %~ TodoList.update msg'
        CreateTodoFromModel -> model & createTodoFromModel

createTodoFromModel :: Model -> Model
createTodoFromModel model = model & createTodo todoText & navigateHome & clearTitleText
  where
    createTodo t = TodoUi.Update.update (UpdateTodoList $ CreateTodo t)
    navigateHome = TodoUi.Update.update (NavigateTo Homepage)
    todoText = editorText $ model^.mCreateTodoForm^.editTitle
    clearTitleText = mCreateTodoForm.editTitle %~ clearEditor
