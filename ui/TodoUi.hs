module TodoUi
    ( runApp
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import           Brick.Main (halt, continue)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import           Control.Lens hiding (Level)
import qualified Data.Text as Txt
import qualified Graphics.Vty as V
import           TodoList
import           TodoUi.Model
import           TodoUi.Route
import           TodoUi.Types
import           TodoUi.Util
import           TodoUi.View

data Msg
    = NoOp
    | NavigateTo Route
    | UpdateTodoList TodoList.Msg
    | CreateTodoFromModel

runApp :: IO Model
runApp = M.defaultMain theApp (Model Homepage demoList initialCreateTodoForm)

demoList :: TodoList
demoList =
    createNewTodoList "Test"
    & TodoList.update (CreateTodo "Buy milk")
    & TodoList.update (CreateTodo "Buy eggs")

theApp :: M.App Model a CreateTodoFieldNames
theApp =
    M.App { M.appDraw = TodoUi.View.view
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr [ ]
          }

update :: TodoUi.Msg -> Model -> Model
update msg model =
    case msg of
        NoOp -> model
        NavigateTo route -> model & mRoute .~ route
        UpdateTodoList msg' -> model & mTodoList %~ TodoList.update msg'
        CreateTodoFromModel -> createTodoFromModel model

createTodoFromModel :: Model -> Model
createTodoFromModel model = model & createTodo todoText & navigateHome & clearTitleText
  where
    createTodo t = TodoUi.update (UpdateTodoList $ CreateTodo t)
    navigateHome = TodoUi.update (NavigateTo Homepage)
    todoText = Txt.concat $ E.getEditContents $ model^.mCreateTodoForm^.editTitle
    clearTitleText = mCreateTodoForm.editTitle %~ clearEditor

appEvent :: Model -> T.BrickEvent CreateTodoFieldNames e -> T.EventM CreateTodoFieldNames (T.Next Model)
appEvent model (T.VtyEvent e) =
    case model^.mRoute of
        Homepage ->
            case e of
                V.EvKey V.KEsc [] -> halt model
                V.EvKey (V.KChar 'a') [] -> continue $ TodoUi.update (NavigateTo CreateNewTodo) model
                _ -> continue model
        CreateNewTodo ->
            case e of
                V.EvKey V.KEsc [] -> continue $ TodoUi.update (NavigateTo Homepage) model
                V.EvKey V.KEnter [] -> continue $ TodoUi.update CreateTodoFromModel model
                _ -> continue =<< case F.focusGetCurrent (model^.mCreateTodoForm^.focusRing) of
                    Just EditTitle -> T.handleEventLensed model (mCreateTodoForm . editTitle) E.handleEditorEvent e
                    Nothing -> return model
appEvent model _ = continue model
