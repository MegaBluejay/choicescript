{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Run (module Run) where

import Hyper

import AST
import Data.ByteString (ByteString)
import Eval

class (MonadCast m, MonadBounded m) => MonadRun m
