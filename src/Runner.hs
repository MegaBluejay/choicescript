{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Runner (module Runner) where

import Control.Lens hiding (Simple)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Hyper
import System.Random

import AST
import Eval
import Run

data RunCtx = RunCtx
  { _startupScene :: Scene SimpleStartup
  , _scenes :: HashMap SceneName (Scene Simple)
  }

data Scene sim = Scene
  { _lines :: [Annotated Int # CLine sim (Annotated Int)]
  , _labels :: HashMap Label Pos
  }

data SomeScene
  = StartupScene (Scene SimpleStartup)
  | NormalScene (Scene Simple)

data SceneState = SceneState
  { _tempVars :: HashMap Var Val
  , _loc :: Pos
  , _hideReuse :: Bool
  , _scene :: SomeScene
  , _args :: [Val]
  }

data RunState = RunState
  { _globalVars :: HashMap Var Val
  , _icf :: Bool
  , _usedOptions :: IntSet
  , _randomState :: StdGen
  , _stack :: NonEmpty SceneState
  , _usedRandomScenes :: HashSet SceneName
  , _achs :: HashMap Achievement (AchData (Annotated Int), Bool)
  }

makeFieldsNoPrefix ''RunCtx
makeFieldsNoPrefix ''RunState
makeFieldsNoPrefix ''SceneState
makeFieldsNoPrefix ''Scene
makePrisms ''SomeScene

curScene :: Lens' RunState SceneState
curScene = stack . head1

instance HasLabels SomeScene (HashMap Label Pos) where
  labels = someSceneAsEither . choosing labels labels

someSceneAsEither :: Iso' SomeScene (Either (Scene SimpleStartup) (Scene Simple))
someSceneAsEither =
  iso
    ( \case
        StartupScene sts -> Left sts
        NormalScene ns -> Right ns
    )
    (either StartupScene NormalScene)

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
    use (curScene . tempVars . at var) >>= \case
      Just val -> pure val
      Nothing ->
        use (globalVars . at var) >>= \case
          Just val -> pure val
          Nothing -> throwRunError $ RunVarNotFound var

instance InnerRun m => MonadCast (ScopedT Int (RunT m)) where
  throwTypeError = throwEvalError . EvalTypeError

instance InnerRun m => MonadBounded (ScopedT Int (RunT m)) where
  throwBoundsError = throwEvalError . EvalOutOfBounds

instance InnerRun m => MonadVar (ScopedT Int (RunT m)) where
  getVar = lift . getVar

instance InnerRun m => MonadEval (ScopedT Int (RunT m)) where
  throwEvalError e = do
    sc <- getScope
    lift $ throwRunError $ EvalError (sc, e)

openScene :: InnerRun m => SceneName -> RunT m SceneState
openScene sn = do
  theScene <- case sn of
    SN "startup" -> Just . StartupScene <$> view startupScene
    _ -> fmap NormalScene <$> view (scenes . at sn)
  case theScene of
    Just s -> pure $ SceneState mempty (P 0) False s []
    Nothing -> throwRunError $ SceneNotFound sn

instance InnerRun m => MonadRun (RunT m) where
  throwRunError = throwScopedError

  runTopEval = topEval

  jumpOut mcm pos = do
    allowed <- use icf
    let fake = mcm == Just FakeChoiceMode
    if allowed || fake
      then jumpPos pos
      else throwRunError NoImp

  jumpPos pos = curScene . loc .= pos

  optionUsed id' = do
    used <- use $ usedOptions . at id'
    case used of
      Just _ -> pure True
      Nothing -> False <$ (usedOptions . at id' .= Just ())

  getGlobalHideReuse = use $ curScene . hideReuse

  setGlobalHideReuse = curScene . hideReuse .= True

  tempVar var val = do
    g <- isJust <$> use (globalVars . at var)
    t <- isJust <$> use (curScene . tempVars . at var)
    if g || t
      then throwRunError $ VarAlreadyExists var
      else curScene . tempVars . at var .= Just val

  setVar var val =
    use (curScene . tempVars . at var) >>= \case
      Just _ -> curScene . tempVars . at var .= Just val
      Nothing ->
        use (globalVars . at var) >>= \case
          Just _ -> globalVars . at var .= Just val
          Nothing -> throwRunError $ RunVarNotFound var

  deleteVar var =
    use (curScene . tempVars . at var) >>= \case
      Just _ -> curScene . tempVars . at var .= Nothing
      Nothing -> throwRunError $ TempNotFound var

  rand lo hi = do
    (a, g) <- uniformR (lo, hi) <$> use randomState
    randomState .= g
    pure a

  goto label =
    use (curScene . scene . labels . at label) >>= \case
      Just pos -> jumpPos pos
      Nothing -> throwRunError $ LabelNotFound label

  gotoScene sn mlabel = do
    ss <- openScene sn
    curScene .= ss
    mapM_ goto mlabel

  nextLine = curScene . loc %= \(P n) -> P (n + 1)

  goSub lbl vals = do
    SceneState ts pos hr s _ <- use curScene
    stack %= NE.cons (SceneState ts pos hr s vals)
    goto lbl

  goSubScene sn margs = do
    ss <- openScene sn
    stack %= NE.cons ss
    forM_ margs $ \(lbl, vals) -> do
      curScene . args .= vals
      goto lbl

  params =
    use (curScene . args) >>= \case
      [] -> throwRunError NoParams
      vals -> pure $ NE.fromList vals

  return' =
    use stack >>= \case
      (_ :| []) -> throwRunError NoSub
      (_ :| sss) -> stack .= NE.fromList sss

  gotoRandomScene sns = do
    q <- use usedRandomScenes
    let notUsed = filter (`HS.member` q) $ NE.toList sns
    case notUsed of
      [] -> throwRunError AllUsed
      xs -> do
        x <- (xs !!) <$> rand 0 (length xs - 1)
        usedRandomScenes . at x .= Just ()
        gotoScene x Nothing

  achieve ach =
    use (achs . at ach) >>= \case
      Just (ad, _) -> achs . at ach .= Just (ad, True)
      Nothing -> throwRunError $ AchNotFound ach

throwScopedError :: (MonadScoped sc m, MonadError (sc, e) m) => e -> m a
throwScopedError e = getScope >>= throwError . (,e)
