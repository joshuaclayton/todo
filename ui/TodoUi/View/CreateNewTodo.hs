module TodoUi.View.CreateNewTodo
    ( view
    , styles
    ) where

import qualified Brick.Focus as F
import qualified Brick.Types as T
import           Brick.Widgets.Core ((<+>), txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens.Operators
import qualified Graphics.Vty as V
import           TodoUi.Types
import           TodoUi.Util (Styles, on)

view :: Model -> [T.Widget TodoEvent]
view m = [ui]
  where
    ui = txt "Create a new todo: " <+> e1
    e1 = F.withFocusRing (m^.mCreateTodoForm^.focusRing) E.renderEditor (m^.mCreateTodoForm^.editTitle)

styles :: Styles
styles =
    [ (E.editAttr, V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]
