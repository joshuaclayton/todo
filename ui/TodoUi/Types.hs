{-# LANGUAGE TemplateHaskell #-}

module TodoUi.Types where

import qualified Brick.Focus as F
import           Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens (makeLenses)
import           Data.Text (Text, unlines)

data CreateTodoFieldNames
    = EditTitle
    deriving (Eq, Ord, Show)

data CreateTodoForm = CreateTodoForm
    { _focusRing :: F.FocusRing CreateTodoFieldNames
    , _editTitle :: E.Editor Text CreateTodoFieldNames
    }

makeLenses ''CreateTodoForm

initialCreateTodoForm :: CreateTodoForm
initialCreateTodoForm =
    CreateTodoForm
        (F.focusRing [EditTitle])
        (E.editor EditTitle (txt . Data.Text.unlines) Nothing "")
