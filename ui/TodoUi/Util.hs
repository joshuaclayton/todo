module TodoUi.Util
    ( clearEditor
    ) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text.Zipper as Z

clearEditor :: Monoid a => E.Editor a b -> E.Editor a b
clearEditor = E.applyEdit Z.clearZipper
