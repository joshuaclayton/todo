module TodoUi
    ( runApp
    ) where

import qualified Brick.AttrMap as A
import           Brick.BChan
import qualified Brick.Focus as F
import           Brick.Main (halt, continue)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Lens hiding (Level)
import           Control.Monad (forever)
import           Data.MonoTraversable (omap)
import           Data.Time (getCurrentTime, addUTCTime)
import qualified Data.Time.Distance as D
import qualified Graphics.Vty as V
import           Todo
import           TodoList
import           TodoUi.Types
import           TodoUi.Update
import           TodoUi.Util
import           TodoUi.View

runApp :: IO Model
runApp = do
    now <- getCurrentTime

    chan <- newBChan 10

    _ <- forkIO $ forever $ do
        now' <- getCurrentTime
        writeBChan chan $ UpdateCurrentTime now'
        threadDelay 10000

    M.customMain (V.mkVty V.defaultConfig) (Just chan) theApp (initialModel now)
  where
    initialModel t = Model
        { _mRoute = Homepage
        , _mTodoList = demoList
        , _mCreateTodoForm = initialCreateTodoForm
        , _mSelectedItem = Nothing
        , _mNow = t
        }

demoList :: TodoList
demoList =
    createNewTodoList "Test"
    & TodoList.update (CreateTodo "Buy milk")
    & TodoList.update (CreateTodo "Buy eggs")

theApp :: M.App Model Effect TodoEvent
theApp =
    M.App { M.appDraw = TodoUi.View.view
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr TodoUi.View.styles
          }

addTimeToDueDate :: Model -> Integer -> D.TimeUnit -> Todo -> Todo.Msg
addTimeToDueDate m i u t =
    case tDueDate t of
        Nothing -> UpdateDueDate $ DueDate $ addTime u (m^.mNow)
        Just d -> UpdateDueDate $ omap (addTime u) d
  where
    addTime D.Minute = addUTCTime (fromInteger $ 60*i)
    addTime D.Hour = addUTCTime (fromInteger $ 60*60*i)
    addTime D.Day = addUTCTime (fromInteger $ 60*60*24*i)
    addTime _ = addUTCTime 0

appEvent :: Model -> T.BrickEvent TodoEvent Effect -> T.EventM TodoEvent (T.Next Model)
appEvent model (T.AppEvent (UpdateCurrentTime t)) = continue $ model & mNow .~ t
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
                V.EvKey (V.KChar 'm') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model 1 D.Minute)
                V.EvKey (V.KChar 'h') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model 1 D.Hour)
                V.EvKey (V.KChar 'd') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model 1 D.Day)
                V.EvKey (V.KChar 'M') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model (-1) D.Minute)
                V.EvKey (V.KChar 'H') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model (-1) D.Hour)
                V.EvKey (V.KChar 'D') [] -> continue $ withSelectedTodo' model (addTimeToDueDate model (-1) D.Day)
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
withSelectedTodo model msg = withSelectedTodo' model (const msg)

withSelectedTodo' :: Model -> (Todo -> Todo.Msg) -> Model
withSelectedTodo' model msg = maybe model updateTodo mselected
  where
    mselected = snd <$> L.listSelectedElement (modelToList model)
    updateTodo = flip TodoUi.Update.update model . updateMsg
    updateMsg t = UpdateTodoList $ UpdateTodo (msg t) $ tId t
