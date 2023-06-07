{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Run (module Run) where

import Control.Lens
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Hyper

import AST
import Eval

data RunError
  = NoOptions

class (MonadCast m, MonadBounded m) => MonadRun m where
  throwRunError :: RunError -> m a

  jumpOut :: Maybe ChoiceMode -> Pos -> m ()

  output :: ByteString -> m ()

  runTopEval :: TopEvalable a => a -> m (EvaledType (TopHyper a))

  jumpPos :: Pos -> m ()

  optionUsed :: Int -> m Bool
  globalHideReuse :: m Bool

  choice :: NonEmpty OutOption -> m ()

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
runCommand (CChoice opts) = do
  opts' <- catMaybes . NE.toList <$> mapM toOutOption opts
  case opts' of
    [] -> throwRunError NoOptions
    _ -> choice $ NE.fromList opts'
runCommand (JumpUnless e pos) = do
  val <- runTopEval e
  b <- asBool val
  unless b $ jumpPos pos

data OutOption = OutOption
  { _selectable :: Bool
  , _text :: ByteString
  , _loc :: Pos
  }

toOutOption :: MonadRun m => COption (Annotated Int) -> m (Maybe OutOption)
toOutOption opt = do
  globalHR <- globalHideReuse
  isUsed <- optionUsed $ opt ^. optionId
  ifOk <- maybe (pure True) (runTopEval >=> asBool) (opt ^. ifMod)
  sifOk <- maybe (pure True) (runTopEval >=> asBool) (opt ^. selectableIf)
  let hide = (isUsed && (globalHR && not (opt ^. allowReuse) || opt ^. hideReuse)) || not ifOk
      gray = (isUsed && (opt ^. disableReuse)) || not sifOk
  txt <- runTopEval $ opt ^. optionText
  pure $ if hide then Nothing else Just $ OutOption (not gray) txt (opt ^. loc)
