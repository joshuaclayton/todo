module TodoUi.Util
    ( Styles
    , clearEditor
    , editorText
    , modelToList
    , on
    ) where

import qualified Brick.AttrMap as A
import           Brick.Util (on)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Control.Lens.Operators
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Todo
import           TodoList (todos)
import           TodoUi.Types (Model, TodoEvent(RenderTodoList), mTodoList, mSelectedItem)

type Styles = [(A.AttrName, V.Attr)]

clearEditor :: Monoid a => E.Editor a b -> E.Editor a b
clearEditor = E.applyEdit Z.clearZipper

editorText :: E.Editor T.Text a -> T.Text
editorText = T.concat . E.getEditContents

modelToList :: Model -> L.List TodoEvent Todo
modelToList m = baseList & updateSelected (m^.mSelectedItem)
  where
    items = todos $ m^.mTodoList
    baseList = L.list (RenderTodoList "primary") (V.fromList items) 1
    updateSelected Nothing list = list
    updateSelected (Just pos) list = L.listMoveTo pos list
