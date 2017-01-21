module TodoUi.View.Homepage
    ( view
    ) where

import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import           Control.Lens.Operators
import           Data.Monoid ((<>))
import           Todo
import           TodoList
import           TodoUi.Model

view :: Model -> [T.Widget a]
view m = [ui]
  where
    items = todos $ m^.mTodoList
    textList = map (txt . ("+ " <>) . tTitle) items
    ui = vBox $
            [ vLimit 1 $ C.center $ str "Todos"
            , B.hBorder
            , txt $ m^.mTodoList^.tlTitle
            ] ++ textList
