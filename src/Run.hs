{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Run (module Run) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Hyper

import AST
import Eval

data RunError

class (MonadCast m, MonadBounded m) => MonadRun m where
  throwRunError :: RunError -> m a

  jumpOut :: Maybe ChoiceMode -> Pos -> m ()

  output :: ByteString -> m ()

  runTopEval :: TopEvalable a => a -> m (EvaledType (TopHyper a))

  jumpPos :: Pos -> m ()

  optionUsed :: Int -> m Bool
  hideReuse :: m Bool

class RunnableSimple sim where
  runSimple :: MonadRun m => sim (Annotated Int) -> m ()

runCLine :: (MonadRun m, RunnableSimple sim) => CLine sim (Annotated Int) # h -> m ()
runCLine (CLoc line) = runLine line
runCLine (JumpOut mcm pos) = jumpOut mcm pos

runLine :: (MonadRun m, RunnableSimple sim) => Line (CCommand sim) (Annotated Int) # h -> m ()
runLine EmptyLine = output "\n"
runLine (Text s) = runTopEval s >>= output
runLine (Command cmd) = runCommand cmd

runCommand :: (MonadRun m, RunnableSimple sim) => CCommand sim (Annotated Int) # h -> m ()
runCommand (CSimple sim) = runSimple sim
runCommand (CChoice opts) = undefined
runCommand (JumpUnless e pos) = do
  val <- runTopEval e
  b <- asBool val
  unless b $ jumpPos pos

data OutOption = OutOption
  { _selectable :: Bool
  , _text :: ByteString
  , _pos :: Pos
  }

toOutOption :: MonadRun m => Option (Const Pos) (Annotated Int) # h -> m (Maybe OutOption)
toOutOption opt = do
  globalHR <- hideReuse
  undefined
