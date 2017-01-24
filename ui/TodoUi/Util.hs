module TodoUi.Util
    ( clearEditor
    , editorText
    ) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z

clearEditor :: Monoid a => E.Editor a b -> E.Editor a b
clearEditor = E.applyEdit Z.clearZipper

editorText :: E.Editor T.Text a -> T.Text
editorText = T.concat . E.getEditContents
