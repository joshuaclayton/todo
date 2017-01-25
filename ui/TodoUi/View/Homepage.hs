module TodoUi.View.Homepage
    ( view
    , styles
    ) where

import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import           Control.Lens.Operators
import           Data.Monoid ((<>))
import qualified Graphics.Vty as V
import           Todo
import           TodoList
import           TodoUi.Types (Model, TodoEvent, mTodoList)
import           TodoUi.Util

view :: Model -> [T.Widget TodoEvent]
view m = [ui]
  where
    ui = vBox
            [ vLimit 1 $ C.center $ str "Todos"
            , B.hBorder
            , txt $ m^.mTodoList^.tlTitle
            , L.renderList listDrawElement True $ modelToList m
            ]

listDrawElement :: Bool -> Todo -> T.Widget a
listDrawElement _ todo = txt $ "+ " <> tTitle todo

styles :: Styles
styles =
    [ (L.listAttr, V.white `on` V.blue)
    , (L.listSelectedAttr, V.blue `on` V.white)
    ]
