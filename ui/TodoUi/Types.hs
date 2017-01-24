{-# LANGUAGE TemplateHaskell #-}

module TodoUi.Types where

import qualified Brick.Focus as F
import           Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens (makeLenses)
import           Data.Text (Text, unlines)
import           TodoList (TodoList)

data Route
    = Homepage
    | CreateNewTodo

data TodoEvent
    = EditTitle
    deriving (Eq, Ord, Show)

data CreateTodoForm = CreateTodoForm
    { _focusRing :: F.FocusRing TodoEvent
    , _editTitle :: E.Editor Text TodoEvent
    }

makeLenses ''CreateTodoForm

data Model = Model
    { _mRoute :: Route
    , _mTodoList :: TodoList
    , _mCreateTodoForm :: CreateTodoForm
    }

makeLenses ''Model

initialCreateTodoForm :: CreateTodoForm
initialCreateTodoForm =
    CreateTodoForm
        (F.focusRing [EditTitle])
        (E.editor EditTitle (txt . Data.Text.unlines) Nothing "")
