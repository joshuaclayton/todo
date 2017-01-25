module TodoUi.View
    ( view
    , styles
    ) where

import qualified Brick.Types as T
import           Control.Lens.Operators
import           Data.Monoid ((<>))
import           TodoUi.Types
import           TodoUi.Util (Styles)
import qualified TodoUi.View.CreateNewTodo as CNT
import qualified TodoUi.View.Homepage as H

view :: Model -> [T.Widget TodoEvent]
view model =
    case model^.mRoute of
        Homepage -> H.view model
        CreateNewTodo -> CNT.view model

styles :: Styles
styles = H.styles <> CNT.styles
