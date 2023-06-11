{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Run (module Run) where

import Control.Lens hiding (Simple, op)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Hyper

import AST
import Eval

data RunError
  = NoOptions
  | RunTypeError TypeError
  | RunOutOfBounds OutOfBounds
  | RunVarNotFound Var
  | NoImp
  | RunEvalError (Int, EvalError)
  | VarAlreadyExists Var
  | TempNotFound Var
  | LabelNotFound Label
  | SceneNotFound SceneName
  | NoParams
  | NoSub
  | AllUsed
  | AchNotFound Achievement
  | EvalError (Int, EvalError)

data OutStat
  = OutTextStat ByteString ByteString
  | OutPercentStat ByteString Int
  | OutOpposedStat ByteString ByteString Int

class Monad m => InnerRun m where
  output :: ByteString -> m ()
  choice :: NonEmpty OutOption -> m OutOption
  inputNumber :: Int -> Int -> m Int
  inputText :: m ByteString
  finish :: ByteString -> m ()
  pageBreak :: ByteString -> m ()
  ending :: m ()
  statChart :: NonEmpty OutStat -> m ()

instance (MonadTrans t, Monad (t m), InnerRun m) => InnerRun (t m) where
  output = lift . output
  choice = lift . choice
  inputNumber = lift .: inputNumber
  inputText = lift inputText
  finish = lift . finish
  pageBreak = lift . pageBreak
  ending = lift ending
  statChart = lift . statChart

toOutHelper :: MonadRun m => ByteString -> m Val
toOutHelper = getVar . V . C.map toLower

toOutNamedStat :: MonadRun m => NamedStat -> m (ByteString, Int)
toOutNamedStat (JustVar v) = do
  val <- toOutHelper v
  n <- asInt val
  pure (v, n)
toOutNamedStat (Named v name) = do
  val <- getVar v
  n <- asInt val
  pure (name, n)

toOutStat :: MonadRun m => Stat -> m OutStat
toOutStat (TextStat t) = do
  let var = V $ C.map toLower t
  val <- getVar var
  pure $ OutTextStat t (printVal val)
toOutStat (PercentStat ns) = uncurry OutPercentStat <$> toOutNamedStat ns
toOutStat (OpposedStat ns right) = do
  (left, n) <- toOutNamedStat ns
  pure $ OutOpposedStat left right n

class (InnerRun m, MonadCast m, MonadBounded m, MonadVar m) => MonadRun m where
  throwRunError :: RunError -> m a

  jumpOut :: Maybe ChoiceMode -> Pos -> m ()

  runTopEval :: TopEvalable a => a -> m (EvaledType (TopHyper a))

  jumpPos :: Pos -> m ()

  optionUsed :: Int -> m Bool
  getGlobalHideReuse :: m Bool
  setGlobalHideReuse :: m ()

  tempVar :: Var -> Val -> m ()
  setVar :: Var -> Val -> m ()
  deleteVar :: Var -> m ()

  rand :: Int -> Int -> m Int

  goto :: Label -> m ()
  gotoScene :: SceneName -> Maybe Label -> m ()
  goSub :: Label -> [Val] -> m ()
  goSubScene :: SceneName -> Maybe (Label, [Val]) -> m ()
  params :: m (NonEmpty Val)
  return' :: m ()
  gotoRandomScene :: NonEmpty SceneName -> m ()

  achieve :: Achievement -> m ()

  nextLine :: m ()

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
runLine EmptyLine = output "\n" *> nextLine
runLine (Text s) = (runTopEval s >>= output) *> nextLine
runLine (Command cmd) = runCommand cmd

runCommand :: (MonadRun m, RunnableSimple sim m) => CCommand sim (Annotated Int) # h -> m ()
runCommand (CSimple sim) = runSimple sim
runCommand (CChoice opts) = do
  opts' <- catMaybes . NE.toList <$> mapM toOutOption opts
  case opts' of
    [] -> throwRunError NoOptions
    _ -> do
      OutOption _ _ pos <- choice $ NE.fromList opts'
      jumpPos pos
runCommand (JumpUnless e pos) = do
  val <- runTopEval e
  b <- asBool val
  if b then nextLine else jumpPos pos

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
  runSimple HideReuse = setGlobalHideReuse *> nextLine
  runSimple (Temp v e) = (runTopEval e >>= tempVar v) *> nextLine
  runSimple (Set target se) = do
    var <- runTopEval target
    val <- runSetExpr var se
    setVar var val
    nextLine
  runSimple (Delete var) = deleteVar var *> nextLine
  runSimple (InputNumber target lo hi) = do
    var <- runTopEval target
    n <- inputNumber lo hi
    setVar var $ IntVal n
    nextLine
  runSimple (InputText target) = do
    var <- runTopEval target
    txt <- inputText
    setVar var $ StrVal txt
    nextLine
  runSimple (Print target) = (runTopEval target >>= getVar >>= output . printVal) *> nextLine
  runSimple (Rand target lo hi) = do
    var <- runTopEval target
    n <- rand lo hi
    setVar var $ IntVal n
    nextLine
  runSimple (Goto target) = runTopEval target >>= goto
  runSimple (GotoScene scene mtarget) = mapM runTopEval mtarget >>= gotoScene scene
  runSimple (Gosub (SubArgs target es)) = do
    label <- runTopEval target
    vals <- mapM runTopEval es
    nextLine
    goSub label vals
  runSimple (GosubScene scene margs) = do
    args <- forM margs $ \(SubArgs target es) -> do
      label <- runTopEval target
      vals <- mapM runTopEval es
      pure (label, vals)
    nextLine
    goSubScene scene args
  runSimple (Params targets) = do
    vars <- mapM runTopEval targets
    vals <- params
    sequence_ (NE.zipWith setVar vars vals)
    nextLine
  runSimple Return = return'
  runSimple (GotoRandomScene scenes) = gotoRandomScene scenes
  runSimple (Finish mstr) =
    (maybe (pure "Next Chapter") runTopEval mstr >>= finish) *> nextLine
  runSimple LineBreak = output "\n" *> nextLine
  runSimple (PageBreak mstr) =
    (maybe (pure "Next") runTopEval mstr >>= pageBreak) *> nextLine
  runSimple (StatChart stats) = (mapM toOutStat stats >>= statChart) *> nextLine
  runSimple (Achieve ach) = achieve ach *> nextLine
  runSimple Ending = ending

instance MonadRunStartup m => RunnableSimple SimpleStartup m where
  runSimple (NormalSimple normal) = runSimple normal
  runSimple (Create var e) = (runTopEval e >>= createVar var) *> nextLine
  runSimple (SceneLst scenes) = sceneList scenes *> nextLine
  runSimple (Title str) = (runTopEval str >>= title) *> nextLine
  runSimple (Author str) = (runTopEval str >>= author) *> nextLine
  runSimple (Achievement ach achData) = achievement ach achData *> nextLine

runSetExpr :: MonadRun m => Var -> SetExpr (Annotated Int) -> m Val
runSetExpr _ (NormalSet e) = runTopEval e
runSetExpr var (ModSet op e) = do
  a <- getVar var
  b <- runTopEval e
  evalBinOp op a b
