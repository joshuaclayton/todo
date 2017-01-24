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
import qualified Graphics.Vty as V
import           TodoList
import           TodoUi.Types
import           TodoUi.Update
import           TodoUi.View

runApp :: IO Model
runApp = M.defaultMain theApp (Model Homepage demoList initialCreateTodoForm)

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
          , M.appAttrMap = const $ A.attrMap V.defAttr [ ]
          }

appEvent :: Model -> T.BrickEvent TodoEvent e -> T.EventM TodoEvent (T.Next Model)
appEvent model (T.VtyEvent e) =
    case model^.mRoute of
        Homepage ->
            case e of
                V.EvKey V.KEsc [] -> halt model
                V.EvKey (V.KChar 'a') [] -> continue $ TodoUi.Update.update (NavigateTo CreateNewTodo) model
                _ -> continue model
        CreateNewTodo ->
            case e of
                V.EvKey V.KEsc [] -> continue $ TodoUi.Update.update (NavigateTo Homepage) model
                V.EvKey V.KEnter [] -> continue $ TodoUi.Update.update CreateTodoFromModel model
                _ -> continue =<< case F.focusGetCurrent (model^.mCreateTodoForm^.focusRing) of
                    Just EditTitle -> T.handleEventLensed model (mCreateTodoForm . editTitle) E.handleEditorEvent e
                    Nothing -> return model
appEvent model _ = continue model
