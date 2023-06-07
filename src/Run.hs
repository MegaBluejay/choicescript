{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Run (module Run) where

import Control.Lens hiding (Simple, op)
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

class (MonadCast m, MonadBounded m, MonadVar m) => MonadRun m where
  throwRunError :: RunError -> m a

  jumpOut :: Maybe ChoiceMode -> Pos -> m ()

  output :: ByteString -> m ()

  runTopEval :: TopEvalable a => a -> m (EvaledType (TopHyper a))

  jumpPos :: Pos -> m ()

  optionUsed :: Int -> m Bool
  getGlobalHideReuse :: m Bool
  setGlobalHideReuse :: m ()

  choice :: NonEmpty OutOption -> m ()

  tempVar :: Var -> Val -> m ()
  setVar :: Var -> Val -> m ()
  deleteVar :: Var -> m ()

  inputNumber :: Var -> Int -> Int -> m ()
  inputText :: Var -> m ()
  rand :: Int -> Int -> m Int

  goto :: Label -> m ()
  gotoScene :: SceneName -> Maybe Label -> m ()
  goSub :: Label -> [Val] -> m ()
  goSubScene :: SceneName -> Maybe (Label, [Val]) -> m ()
  params :: m (NonEmpty Val)
  return' :: m ()
  gotoRandomScene :: NonEmpty SceneName -> m ()

  finish :: ByteString -> m ()
  pageBreak :: ByteString -> m ()
  ending :: m ()

  statChart :: NonEmpty Stat -> m ()

  achieve :: Achievement -> m ()
  checkAchievements :: m ()

class MonadRun m => MonadRunStartup m where
  createVar :: Var -> Val -> m ()
  sceneList :: NonEmpty SceneName -> m ()
  title :: ByteString -> m ()
  author :: ByteString -> m ()
  achievement :: Achievement -> AchData (Annotated Int) -> m ()

class RunnableSimple sim m where
  runSimple :: sim (Annotated Int) -> m ()

runCLine :: (MonadRun m, RunnableSimple sim m) => CLine sim (Annotated Int) # h -> m ()
runCLine (CLoc line) = runLine line
runCLine (JumpOut mcm pos) = jumpOut mcm pos

runLine :: (MonadRun m, RunnableSimple sim m) => Line (CCommand sim) (Annotated Int) # h -> m ()
runLine EmptyLine = output "\n"
runLine (Text s) = runTopEval s >>= output
runLine (Command cmd) = runCommand cmd

runCommand :: (MonadRun m, RunnableSimple sim m) => CCommand sim (Annotated Int) # h -> m ()
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
  globalHR <- getGlobalHideReuse
  isUsed <- optionUsed $ opt ^. optionId
  ifOk <- maybe (pure True) (runTopEval >=> asBool) (opt ^. ifMod)
  sifOk <- maybe (pure True) (runTopEval >=> asBool) (opt ^. selectableIf)
  let hide = (isUsed && (globalHR && not (opt ^. allowReuse) || opt ^. hideReuse)) || not ifOk
      gray = (isUsed && (opt ^. disableReuse)) || not sifOk
  txt <- runTopEval $ opt ^. optionText
  pure $ if hide then Nothing else Just $ OutOption (not gray) txt (opt ^. loc)

instance MonadRun m => RunnableSimple Simple m where
  runSimple HideReuse = setGlobalHideReuse
  runSimple (Temp v e) = runTopEval e >>= tempVar v
  runSimple (Set target se) = do
    var <- runTopEval target
    val <- runSetExpr var se
    setVar var val
  runSimple (Delete var) = deleteVar var
  runSimple (InputNumber target lo hi) = do
    var <- runTopEval target
    inputNumber var lo hi
  runSimple (InputText target) = runTopEval target >>= inputText
  runSimple (Print target) = runTopEval target >>= getVar >>= output . printVal
  runSimple (Rand target lo hi) = do
    var <- runTopEval target
    n <- rand lo hi
    setVar var $ IntVal n
  runSimple (Goto target) = runTopEval target >>= goto
  runSimple (GotoScene scene mtarget) = mapM runTopEval mtarget >>= gotoScene scene
  runSimple (Gosub (SubArgs target es)) = do
    label <- runTopEval target
    vals <- mapM runTopEval es
    goSub label vals
  runSimple (GosubScene scene margs) = do
    args <- forM margs $ \(SubArgs target es) -> do
      label <- runTopEval target
      vals <- mapM runTopEval es
      pure (label, vals)
    goSubScene scene args
  runSimple (Params targets) = do
    vars <- mapM runTopEval targets
    vals <- params
    sequence_ (NE.zipWith setVar vars vals)
  runSimple Return = return'
  runSimple (GotoRandomScene scenes) = gotoRandomScene scenes
  runSimple (Finish mstr) =
    maybe (pure "Next Chapter") runTopEval mstr >>= finish
  runSimple LineBreak = output "\n"
  runSimple (PageBreak mstr) =
    maybe (pure "Next") runTopEval mstr >>= pageBreak
  runSimple (StatChart stats) = statChart stats
  runSimple (Achieve ach) = achieve ach
  runSimple CheckAchievements = checkAchievements
  runSimple Ending = ending

instance MonadRunStartup m => RunnableSimple SimpleStartup m where
  runSimple (NormalSimple normal) = runSimple normal
  runSimple (Create var e) = runTopEval e >>= createVar var
  runSimple (SceneLst scenes) = sceneList scenes
  runSimple (Title str) = runTopEval str >>= title
  runSimple (Author str) = runTopEval str >>= author
  runSimple (Achievement ach achData) = achievement ach achData

runSetExpr :: MonadRun m => Var -> SetExpr (Annotated Int) -> m Val
runSetExpr _ (NormalSet e) = runTopEval e
runSetExpr var (ModSet op e) = do
  a <- getVar var
  b <- runTopEval e
  evalBinOp op a b
