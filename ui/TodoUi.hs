module TodoUi
    ( runApp
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import           Brick.Main (halt, continue)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Control.Lens hiding (Level)
import qualified Graphics.Vty as V
import           Todo
import           TodoList
import           TodoUi.Types
import           TodoUi.Update
import           TodoUi.Util
import           TodoUi.View

runApp :: IO Model
runApp = M.defaultMain theApp initialModel
  where
    initialModel = Model
        { _mRoute = Homepage
        , _mTodoList = demoList
        , _mCreateTodoForm = initialCreateTodoForm
        , _mSelectedItem = Nothing
        }

demoList :: TodoList
demoList =
    createNewTodoList "Test"
    & TodoList.update (CreateTodo "Buy milk")
    & TodoList.update (CreateTodo "Buy eggs")

theApp :: M.App Model a TodoEvent
theApp =
    M.App { M.appDraw = TodoUi.View.view
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr TodoUi.View.styles
          }

appEvent :: Model -> T.BrickEvent TodoEvent e -> T.EventM TodoEvent (T.Next Model)
appEvent model (T.VtyEvent e) =
    case model^.mRoute of
        Homepage ->
            case e of
                V.EvKey V.KEsc [] -> halt model
                V.EvKey (V.KChar 'a') [] -> continue $ TodoUi.Update.update (NavigateTo CreateNewTodo) model
                V.EvKey (V.KChar '+') [] -> continue $ withSelectedTodo model IncreasePriority
                V.EvKey (V.KChar '_') [] -> continue $ withSelectedTodo model DecreasePriority
                V.EvKey (V.KChar '=') [] -> continue $ withSelectedTodo model IncreasePriority
                V.EvKey (V.KChar '-') [] -> continue $ withSelectedTodo model DecreasePriority
                V.EvKey (V.KChar '1') [] -> continue $ withSelectedTodo model $ SetPriority Low
                V.EvKey (V.KChar '2') [] -> continue $ withSelectedTodo model $ SetPriority Medium
                V.EvKey (V.KChar '3') [] -> continue $ withSelectedTodo model $ SetPriority High
                ev -> continue =<< do
                    newList <- L.handleListEvent ev (modelToList model)
                    return $ model & mSelectedItem .~ (newList ^. L.listSelectedL)
        CreateNewTodo ->
            case e of
                V.EvKey V.KEsc [] -> continue $ TodoUi.Update.update (NavigateTo Homepage) model
                V.EvKey V.KEnter [] -> continue $ TodoUi.Update.update CreateTodoFromModel model
                _ -> continue =<< case F.focusGetCurrent (model^.mCreateTodoForm^.focusRing) of
                    Just EditTitle -> T.handleEventLensed model (mCreateTodoForm . editTitle) E.handleEditorEvent e
                    Nothing -> return model
                    _ -> return model
appEvent model _ = continue model

withSelectedTodo :: Model -> Todo.Msg -> Model
withSelectedTodo model msg = maybe model updateTodo mselected
  where
    mselected = snd <$> L.listSelectedElement (modelToList model)
    updateTodo = flip TodoUi.Update.update model . updateMsg
    updateMsg = UpdateTodoList . UpdateTodo msg . tId
