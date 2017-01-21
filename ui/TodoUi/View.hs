module TodoUi.View
    ( view
    ) where

import qualified Brick.Types as T
import           Control.Lens.Operators
import           TodoUi.Model
import           TodoUi.Route
import           TodoUi.Types
import qualified TodoUi.View.CreateNewTodo as CNT
import qualified TodoUi.View.Homepage as H

view :: Model -> [T.Widget CreateTodoFieldNames]
view model =
    case model^.mRoute of
        Homepage -> H.view model
        CreateNewTodo -> CNT.view model
