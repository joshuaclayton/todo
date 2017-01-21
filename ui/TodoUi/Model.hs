{-# LANGUAGE TemplateHaskell #-}

module TodoUi.Model where

import Control.Lens (makeLenses)
import TodoList (TodoList)
import TodoUi.Route (Route)
import TodoUi.Types (CreateTodoForm)

data Model = Model
    { _mRoute :: Route
    , _mTodoList :: TodoList
    , _mCreateTodoForm :: CreateTodoForm
    }

makeLenses ''Model
