module TodoUi.View.Homepage
    ( view
    , styles
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import           Control.Lens.Operators
import           Data.Monoid ((<>))
import           Data.Time.Distance
import qualified Graphics.Vty as V
import           Todo
import           TodoList
import           TodoUi.Types (Model, TodoEvent, mTodoList, mNow)
import           TodoUi.Util

data Selector
    = StylePriority Todo.TodoPriority

view :: Model -> [T.Widget TodoEvent]
view m = [ui]
  where
    ui = vBox
            [ vLimit 1 $ C.center $ str "Todos"
            , B.hBorder
            , txt $ m^.mTodoList^.tlTitle
            , L.renderList (listDrawElement m) True $ modelToList m
            ]


listDrawElement :: Model -> Bool -> Todo -> T.Widget a
listDrawElement m _ todo =
    withStyle (StylePriority $ tPriority todo) (txt "+ ") <+>
    txt (tTitle todo) <+>
    rightAlign (str dueDate)
  where
    rightAlign = padLeft T.Max
    dueDate = maybe " " (\(DueDate t) -> distanceOfTimeInWords t (m^.mNow)) (tDueDate todo)

styles :: Styles
styles =
    [ (L.listAttr, V.white `on` V.blue)
    , (L.listSelectedAttr, V.blue `on` V.white)
    , (buildStyle (StylePriority High), V.white `on` V.red)
    , (buildStyle (StylePriority Medium), V.white `on` V.yellow)
    , (buildStyle (StylePriority Low), V.white `on` V.green)
    ]

withStyle :: Selector -> T.Widget a -> T.Widget a
withStyle = withAttr . buildStyle

buildStyle :: Selector -> A.AttrName
buildStyle (StylePriority p) = "priority" <> A.attrName (show p)
