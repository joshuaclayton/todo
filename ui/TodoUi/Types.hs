{-# LANGUAGE TemplateHaskell #-}

module TodoUi.Types where

import qualified Brick.Focus as F
import           Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Edit as E
import           Control.Lens (makeLenses)
import           Data.Text (Text, unlines)
import qualified Data.Time as T
import           TodoList (TodoList)

data Route
    = Homepage
    | CreateNewTodo

data TodoEvent
    = EditTitle
    | RenderTodoList Text
    deriving (Eq, Ord, Show)

data Effect = UpdateCurrentTime T.UTCTime

data CreateTodoForm = CreateTodoForm
    { _focusRing :: F.FocusRing TodoEvent
    , _editTitle :: E.Editor Text TodoEvent
    }

makeLenses ''CreateTodoForm

data Model = Model
    { _mRoute :: Route
    , _mTodoList :: TodoList
    , _mCreateTodoForm :: CreateTodoForm
    , _mSelectedItem :: Maybe Int
    , _mNow :: T.UTCTime
    }

makeLenses ''Model

initialCreateTodoForm :: CreateTodoForm
initialCreateTodoForm =
    CreateTodoForm
        (F.focusRing [EditTitle])
        (E.editor EditTitle (txt . Data.Text.unlines) (Just 1) "")
