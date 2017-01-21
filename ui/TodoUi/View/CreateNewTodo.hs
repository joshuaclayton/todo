module TodoUi.View.CreateNewTodo
    ( view
    ) where

import qualified Brick.Focus as F
import qualified Brick.Types as T
import           Brick.Widgets.Core ((<+>), txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens.Operators
import           TodoUi.Model
import           TodoUi.Types

view :: Model -> [T.Widget CreateTodoFieldNames]
view m = [ui]
  where
    ui = txt "Create a new todo: " <+> e1
    e1 = F.withFocusRing (m^.mCreateTodoForm^.focusRing) E.renderEditor (m^.mCreateTodoForm^.editTitle)
