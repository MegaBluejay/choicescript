{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Runner (module Runner) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Maybe
import System.Random

import AST
import Eval
import Run

newtype RunCtx = RunCtx
  {_labels :: HashMap Label Pos}

data RunState = RunState
  { _tempVars :: HashMap Var Val
  , _globalVars :: HashMap Var Val
  , _loc :: Pos
  , _icf :: Bool
  , _usedOptions :: IntSet
  , _hideReuse :: Bool
  , _randomState :: StdGen
  }

makeFieldsNoPrefix ''RunCtx
makeFieldsNoPrefix ''RunState

newtype RunT m a = RunT {runRunT :: ScopedT Int (ReaderT RunCtx (StateT RunState (ExceptT (Int, RunError) m))) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader RunCtx, MonadState RunState, MonadError (Int, RunError), MonadScoped Int)

instance MonadTrans RunT where
  lift = RunT . lift . lift . lift . lift

instance InnerRun m => MonadCast (RunT m) where
  throwTypeError = throwRunError . RunTypeError

instance InnerRun m => MonadBounded (RunT m) where
  throwBoundsError = throwRunError . RunOutOfBounds

instance InnerRun m => MonadVar (RunT m) where
  getVar var =
    use (tempVars . at var) >>= \case
      Just val -> pure val
      Nothing ->
        use (globalVars . at var) >>= \case
          Just val -> pure val
          Nothing -> throwRunError $ RunVarNotFound var

instance InnerRun m => MonadRun (RunT m) where
  throwRunError = throwScopedError

  jumpOut mcm pos = do
    allowed <- use icf
    let fake = mcm == Just FakeChoiceMode
    if allowed || fake
      then jumpPos pos
      else throwRunError NoImp

  jumpPos pos = loc .= pos

  optionUsed id' = do
    used <- use $ usedOptions . at id'
    case used of
      Just _ -> pure True
      Nothing -> False <$ (usedOptions . at id' .= Just ())

  getGlobalHideReuse = use hideReuse

  setGlobalHideReuse = hideReuse .= True

  tempVar var val = do
    g <- isJust <$> use (globalVars . at var)
    t <- isJust <$> use (tempVars . at var)
    if g || t
      then throwRunError $ VarAlreadyExists var
      else tempVars . at var .= Just val

  setVar var val =
    use (tempVars . at var) >>= \case
      Just _ -> tempVars . at var .= Just val
      Nothing ->
        use (globalVars . at var) >>= \case
          Just _ -> globalVars . at var .= Just val
          Nothing -> throwRunError $ RunVarNotFound var

  deleteVar var =
    use (tempVars . at var) >>= \case
      Just _ -> tempVars . at var .= Nothing
      Nothing -> throwRunError $ TempNotFound var

  rand lo hi = do
    (a, g) <- uniformR (lo, hi) <$> use randomState
    randomState .= g
    pure a

  goto label =
    view (labels . at label) >>= \case
      Just pos -> jumpPos pos
      Nothing -> throwRunError $ LabelNotFound label

throwScopedError :: (MonadScoped sc m, MonadError (sc, e) m) => e -> m a
throwScopedError e = getScope >>= throwError . (,e)
