module TodoUi.View.CreateNewTodo
    ( view
    , styles
    ) where

import qualified Brick.Focus as F
import qualified Brick.Types as T
import           Brick.Widgets.Core (TextWidth, (<+>), hLimit, txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens.Operators
import qualified Data.Text.Zipper.Generic as Z
import qualified Graphics.Vty as V
import           TodoUi.Types
import           TodoUi.Util (Styles, on)

view :: Model -> [T.Widget TodoEvent]
view m = [ui]
  where
    ui = txt "Create a new todo: " <+> hLimit 40 e1
    e1 =
        buildForm
            (m^.mCreateTodoForm^.focusRing)
            (m^.mCreateTodoForm^.editTitle)

buildForm :: (Z.GenericTextZipper i, TextWidth i)
    => F.FocusRing TodoEvent
    -> E.Editor i TodoEvent
    -> T.Widget TodoEvent
buildForm f = F.withFocusRing f E.renderEditor

styles :: Styles
styles =
    [ (E.editAttr, V.white `on` V.blue)
    , (E.editFocusedAttr, V.blue `on` V.white)
    ]
