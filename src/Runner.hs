{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Runner (
  module Runner,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Bool.Singletons
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.Eq.Singletons
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.String.Singletons
import Data.Vector (Vector)
import Data.Vector qualified as V
import Exinst
import Exinst.Base
import Hyper
import Hyper.Recurse
import Text.Show.Singletons

import AST
import Compiler
import Data.Functor.Identity (Identity (runIdentity))
import Lexer (Caret, Careted (..), Loc (..), LocType, Span, Spanned (..))

$( singletons
    [d|
      data ValTy = IntTy | BoolTy | StrTy
        deriving (Eq, Show)
      |]
 )

type family TypeOf (t :: ValTy) where
  TypeOf IntTy = Int
  TypeOf BoolTy = Bool
  TypeOf StrTy = ByteString

data Val (t :: ValTy) where
  Val :: {getVal :: TypeOf t} -> Val t

deriving instance Eq (TypeOf t) => Eq (Val t)
deriving instance Show (TypeOf t) => Show (Val t)

instance (c (f IntTy), c (f BoolTy), c (f StrTy)) => Dict1 c (f :: ValTy -> k) where
  dict1 = \case
    SIntTy -> Dict
    SBoolTy -> Dict
    SStrTy -> Dict

type Vars = HashMap Var (Some1 Val)

data EvalError
  = EvalVarNotFound Var
  | EvalTypeMismatch ValTy [ValTy]
  | EvalOutOfBounds Int
  deriving (Show)

data EvalCtx = EvalCtx {evalLoc :: Span, evalVars :: Vars}

type family EvaledType (h :: HyperType) where
  EvaledType Expr = Some1 Val
  EvaledType Multirep = ByteString
  EvaledType Str = ByteString
  EvaledType IfMod = IfMod # Evaled
  EvaledType ReuseMod = ReuseMod # Evaled

newtype Evaled h = Evaled {getEvaled :: EvaledType (GetHyperType h)}

class Monad m => MonadCast m where
  throwCastError :: ValTy -> ValTy -> m a

class MonadCast m => MonadEval m where
  throwEvalError :: EvalError -> m a
  askVar :: Var -> m (Some1 Val)

newtype EvalT m a = EvalT {unEvalT :: ReaderT EvalCtx (ExceptT (Spanned EvalError) m) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader EvalCtx, MonadError (Spanned EvalError))

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

type Eval = EvalT Identity

runEvalT :: EvalT m a -> EvalCtx -> m (Either (Spanned EvalError) a)
runEvalT x r = runExceptT $ runReaderT (unEvalT x) r

runEval :: Eval a -> EvalCtx -> Either (Spanned EvalError) a
runEval = runIdentity .: runEvalT

instance Monad m => MonadEval (EvalT m) where
  throwEvalError e = asks ((e :~) . evalLoc) >>= throwError
  askVar var =
    asks ((HM.!? var) . evalVars) >>= \case
      Just val -> pure val
      Nothing -> throwEvalError $ EvalVarNotFound var

instance Monad m => MonadCast (EvalT m) where
  throwCastError got exp = throwEvalError $ EvalTypeMismatch got [exp]

class (LocType h ~ Span, RTraversable h) => Evalable h where
  eval :: (MonadEval m) => h # Evaled -> m (EvaledType h)

class (HTraversable h, HNodesConstraint h (Recursively Evalable)) => PureEvalable h where
  pureEval :: h # Evaled -> EvaledType h

multirepVal :: MonadEval m => Sing t -> Val t -> m Int
multirepVal t (Val v) = case t of
  SIntTy -> pure $ v - 1
  SBoolTy -> pure $ if v then 0 else 1
  SStrTy -> throwEvalError $ EvalTypeMismatch StrTy [IntTy, BoolTy]

instance Evalable Multirep where
  eval (Multirep (Evaled val) ss) = do
    i <- withSome1Sing val multirepVal
    if 0 <= i && i < length ss
      then pure . pureEval $ ss NE.!! i
      else throwEvalError $ EvalOutOfBounds i

valAs :: MonadCast m => Sing t -> Some1 Val -> m (TypeOf t)
valAs t (Some1 t' val) = case t %~ t' of
  Proved Refl -> pure $ getVal val
  Disproved _ -> throwCastError (fromSing t') (fromSing t)

int :: Int -> Val IntTy
int = Val

bool :: Bool -> Val BoolTy
bool = Val

str :: ByteString -> Val StrTy
str = Val

evalUnOp :: MonadCast m => UnOp -> Some1 Val -> m (Some1 Val)
evalUnOp Neg = fmap (someVal SIntTy) . apply SIntTy negate
evalUnOp Not = fmap (someVal SBoolTy) . apply SBoolTy not

apply :: MonadCast m => Sing t1 -> (TypeOf t1 -> x) -> Some1 Val -> m x
apply t1 f x = f <$> valAs t1 x

apply2
  :: MonadCast m
  => Sing t1
  -> Sing t2
  -> (TypeOf t1 -> TypeOf t2 -> x)
  -> Some1 Val
  -> Some1 Val
  -> m x
apply2 t1 t2 f x y = f <$> valAs t1 x <*> valAs t2 y

someVal :: forall t. (SingI t) => Sing t -> TypeOf t -> Some1 Val
someVal _ = some1 . Val @t

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.:)

infixr 8 .:
infixr 8 .:.

arith :: MonadCast m => (Int -> Int -> Int) -> Some1 Val -> Some1 Val -> m (Some1 Val)
arith = fmap (someVal SIntTy) .:. apply2 SIntTy SIntTy

cmp :: MonadCast m => (Int -> Int -> Bool) -> Some1 Val -> Some1 Val -> m (Some1 Val)
cmp = fmap (someVal SBoolTy) .:. apply2 SIntTy SIntTy

logic :: MonadCast m => (Bool -> Bool -> Bool) -> Some1 Val -> Some1 Val -> m (Some1 Val)
logic = fmap (someVal SBoolTy) .:. apply2 SBoolTy SBoolTy

evalBinOp :: MonadEval m => BinOp -> Some1 Val -> Some1 Val -> m (Some1 Val)
evalBinOp Add = arith (+)
evalBinOp Sub = arith (-)
evalBinOp Mul = arith (*)
evalBinOp Div = arith (round @Double @Int .: (/) `on` fromIntegral)
evalBinOp Mod = arith mod
evalBinOp FAdd = arith $ \x y ->
  let x' = fromIntegral x
      y' = fromIntegral y
   in round @Double @Int $ x' + (100 - x') * y' / 100
evalBinOp FSub = arith $ \x y ->
  let x' = fromIntegral x
      y' = fromIntegral y
   in round @Double @Int $ x' - x' * y' / 100
evalBinOp Eq = \(Some1 t1 x) (Some1 t2 y) ->
  some1 . bool <$> case (t1, t2) of
    (SIntTy, SIntTy) -> pure $ x == y
    (SBoolTy, SBoolTy) -> pure $ x == y
    (SStrTy, SStrTy) -> pure $ x == y
    _ -> throwEvalError $ EvalTypeMismatch (fromSing t2) [fromSing t1]
evalBinOp Neq = \x y -> evalBinOp Eq x y >>= evalUnOp Not
evalBinOp Gt = cmp (>)
evalBinOp Lt = cmp (<)
evalBinOp And = logic (&&)
evalBinOp Or = logic (||)
evalBinOp Get = \(Some1 t1 (Val x)) (Some1 t2 (Val y)) ->
  some1 . str <$> case (t1, t2) of
    (SStrTy, SIntTy) -> case x C.!? (y - 1) of
      Just c -> pure $ C.singleton c
      Nothing -> throwEvalError $ EvalOutOfBounds y
    (SStrTy, _) -> throwEvalError $ EvalTypeMismatch (fromSing t2) [IntTy]
    _ -> throwEvalError $ EvalTypeMismatch (fromSing t1) [StrTy]
evalBinOp Cat = fmap (someVal SStrTy) .: apply2 SStrTy SStrTy (<>)

evalFun :: MonadEval m => Fun -> Some1 Val -> m (Some1 Val)
evalFun Length = fmap (someVal SIntTy) . apply SStrTy C.length

printVal :: Sing t -> Val t -> ByteString
printVal t (Val v) = case t of
  SStrTy -> v
  SIntTy -> C.pack $ show v
  SBoolTy -> if v then "true" else "false"

capitalize :: CapMode -> ByteString -> ByteString
capitalize NoCap s = s
capitalize Cap s = case C.uncons s of
  Just (x, xs) -> C.cons (toUpper x) xs
  Nothing -> ""
capitalize Upper s = C.map toUpper s

evalStrPart :: StrPart # Evaled -> ByteString
evalStrPart (Normal s) = s
evalStrPart (SPMultirep (Evaled s)) = s
evalStrPart (Inter capMode (Evaled val)) =
  capitalize capMode $ withSome1Sing val printVal

instance PureEvalable Str where
  pureEval (S ss) = B.concat $ map evalStrPart ss

instance Evalable Expr where
  eval (Constant (Int n)) = pure $ some1 $ Val @IntTy n
  eval (Constant (Bool b)) = pure $ some1 $ Val @BoolTy b
  eval (Str s) = pure $ some1 $ Val @StrTy $ pureEval s
  eval (Var (DirectTarget var)) = askVar var
  eval (Var (RefTarget (Evaled val))) = valAs SStrTy val >>= askVar . V
  eval (UnOp op (Evaled val)) = evalUnOp op val
  eval (BinOp (Evaled x) op (Evaled y)) = evalBinOp op x y
  eval (Fun fun (Evaled x)) = evalFun fun x

hthing
  :: forall w0 w1 h m
   . (Monad m, RTraversable h)
  => (forall n. HRecWitness h n -> n # w1 -> m (w1 # n))
  -> (forall n a. HRecWitness h n -> (n # w0 -> m a) -> w0 # n -> m a)
  -> w0 # h
  -> m (w1 # h)
hthing f g =
  withDict (recurse $ Proxy @(RTraversable h)) $
    g HRecSelf $
      htraverse
        ( Proxy @RTraversable #*# \w -> hthing (f . HRecSub w) (g . HRecSub w)
        )
        >=> f HRecSelf

prepare :: (Monad m, LocType h ~ Span) => (h # Ann Loc -> EvalT m a) -> Ann Loc # h -> EvalT m a
prepare f (Ann (Loc loc) x) = local (\ctx -> ctx{evalLoc = loc}) $ f x

topEval
  :: forall h
   . Recursively Evalable h
  => Vars
  -> Ann Loc # h
  -> Either (Spanned EvalError) (EvaledType h)
topEval vars x@(Ann (Loc loc) _) =
  withDict (recursively $ Proxy @(Evalable h)) $
    runEval
      ( getEvaled
          <$> hthing
            (Proxy @Evalable ##>> fmap Evaled . eval)
            (Proxy @Evalable ##>> prepare)
            x
      )
      (EvalCtx loc vars)

topPureEval
  :: PureEvalable h
  => Vars
  -> h # Ann Loc
  -> Either (Spanned EvalError) (EvaledType h)
topPureEval vars =
  fmap pureEval
    . htraverse
      ( Proxy @(Recursively Evalable)
          #> fmap Evaled
          . topEval vars
      )

evalExprWitness :: Dict (Recursively Evalable Expr)
evalExprWitness = Dict

data RunError
  = EvalError (Spanned EvalError)
  | RunTypeMismatch ValTy [ValTy]
  | RunVarNotFound Var
  | VarConflict Var
  | NoImplicitControlFlow
  | NoActiveSub
  | LabelNotFound Label
  deriving (Show)

data AllVars = AllVars
  { tempVars :: Vars
  , globalVars :: Vars
  }
  deriving (Show)

unifiedVars :: AllVars -> Vars
unifiedVars (AllVars tvs gvs) = HM.union tvs gvs

data RunState = ProgState
  { vars :: AllVars
  , pos :: Pos
  , hiddenOpts :: IntSet
  , hideReuse :: Bool
  , implicitControlFlow :: Bool
  , frames :: [Pos]
  }
  deriving (Show)

data RunCtx simple = RunCtx
  { lbls :: HashMap Label Pos
  , prog :: Vector (CLine simple # Ann Loc)
  , runLoc :: Caret
  }

newtype RunT simple m a = RunT {unRunT :: ReaderT (RunCtx simple) (StateT RunState (ExceptT (Careted RunError) m)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (RunCtx simple), MonadState RunState, MonadError (Careted RunError))

instance MonadTrans (RunT simple) where
  lift = RunT . lift . lift . lift

evalToRun :: MonadRun m => Either (Spanned EvalError) a -> m a
evalToRun = either (throwRunError . EvalError) pure

class Monad m => MonadInter m where
  display :: ByteString -> m ()
  button :: ByteString -> m ()
  choose :: [Some1 (FrontOpt tag)] -> m (FrontOpt tag True)
  inputNumber :: Int -> Int -> m Int
  inputText :: m ByteString

instance MonadInter m => MonadInter (RunT simple m) where
  display = lift . display
  button = lift . button
  choose = lift . choose
  inputNumber = lift .: inputNumber
  inputText = lift inputText

class (MonadInter m, MonadCast m) => MonadRun m where
  throwRunError :: RunError -> m a
  nextLine :: m ()
  jumpToPos :: Pos -> m ()
  jumpToLabel :: Label -> m ()
  withCaret :: LocType h ~ Caret => (h # Ann Loc -> m a) -> Ann Loc # h -> m a
  runTopEval :: Recursively Evalable h => Ann Loc # h -> m (EvaledType h)
  runTopPureEval :: PureEvalable h => h # Ann Loc -> m (EvaledType h)
  tempVar :: Var -> Some1 Val -> m ()
  createVar :: Var -> Some1 Val -> m ()
  setVar :: Var -> Some1 Val -> m ()
  getVar :: Var -> m (Some1 Val)
  deleteVar :: Var -> m ()
  isReused :: Int -> m Bool
  getHideReuse :: m Bool
  setHideReuse :: m ()
  getImplicitControlFlow :: m Bool
  pushFrame :: m ()
  popFrame :: m ()

instance MonadInter m => MonadRun (RunT simple m) where
  throwRunError e = asks ((e :^) . runLoc) >>= throwError

  nextLine = modify $ \ctx -> ctx{pos = pos ctx + 1}

  jumpToPos pos = modify $ \ctx -> ctx{pos = pos}

  jumpToLabel lbl = asks ((HM.!? lbl) . lbls) >>= maybe (throwRunError $ LabelNotFound lbl) jumpToPos

  withCaret f (Ann (Loc loc) x) = local (\ctx -> ctx{runLoc = loc}) $ f x

  runTopEval x = gets (unifiedVars . vars) >>= \vs -> evalToRun $ topEval vs x

  runTopPureEval x = gets (unifiedVars . vars) >>= \vs -> evalToRun $ topPureEval vs x

  tempVar var val = do
    avs <- gets vars
    let gvs = globalVars avs
        tvs = tempVars avs
    if HM.member var gvs || HM.member var tvs
      then throwRunError $ VarConflict var
      else modify $ \st -> st{vars = avs{tempVars = HM.insert var val tvs}}

  createVar var val = do
    avs <- gets vars
    let gvs = globalVars avs
        tvs = globalVars avs
    if HM.member var gvs || HM.member var tvs
      then throwRunError $ VarConflict var
      else modify $ \st -> st{vars = avs{globalVars = HM.insert var val gvs}}

  setVar var val = do
    avs <- gets vars
    let gvs = globalVars avs
    if HM.member var gvs
      then modify $ \st -> st{vars = avs{globalVars = HM.insert var val gvs}}
      else
        let tvs = tempVars avs
         in if HM.member var tvs
              then modify $ \st -> st{vars = avs{tempVars = HM.insert var val tvs}}
              else throwRunError $ RunVarNotFound var

  getVar var = do
    avs <- gets vars
    case globalVars avs HM.!? var of
      Just val -> pure val
      Nothing -> case tempVars avs HM.!? var of
        Just val -> pure val
        Nothing -> throwRunError $ RunVarNotFound var

  deleteVar var = do
    avs <- gets vars
    let gvs = globalVars avs
    if HM.member var gvs
      then modify $ \st -> st{vars = avs{globalVars = HM.delete var gvs}}
      else
        let tvs = tempVars avs
         in if HM.member var tvs
              then modify $ \st -> st{vars = avs{tempVars = HM.delete var tvs}}
              else throwRunError $ RunVarNotFound var

  isReused i = state $ \st ->
    let h = IS.member i $ hiddenOpts st
     in if h then (True, st) else (False, st{hiddenOpts = IS.insert i $ hiddenOpts st})

  getHideReuse = gets hideReuse
  setHideReuse = modify $ \st -> st{hideReuse = True}

  getImplicitControlFlow = gets implicitControlFlow

  pushFrame = gets pos >>= \p -> modify $ \st -> st{frames = p : frames st}
  popFrame = do
    fs <- gets frames
    case fs of
      (f : fs') -> modify (\st -> st{frames = fs'}) *> jumpToPos f
      [] -> throwRunError NoActiveSub

instance MonadInter m => MonadCast (RunT simple m) where
  throwCastError got exp = throwRunError $ RunTypeMismatch got [exp]

runLine :: (RunSimple simple, MonadRun m) => CLine simple # Ann Loc -> m ()
runLine (CLoc cLoc) = runCLoc cLoc
runLine (ImplicitJumpIf pos) = jumpToPos pos
runLine (ImplicitJumpChoice cm pos) = case cm of
  FakeChoiceMode -> jumpToPos pos
  ChoiceMode -> do
    imp <- getImplicitControlFlow
    if imp
      then jumpToPos pos
      else throwRunError NoImplicitControlFlow

runCLoc :: (RunSimple simple, MonadRun m) => Ann Loc # CLoc simple -> m ()
runCLoc = withCaret $ \case
  CFlat flat -> runFlat flat
  JumpUnless e pos -> do
    val <- runTopEval e
    b <- valAs SBoolTy val
    if b then nextLine else jumpToPos pos
  CChoice choice -> runChoice choice

runFlat :: (RunSimple simple, MonadRun m) => FlatLine simple # Ann Loc -> m ()
runFlat (Text s) = runTopPureEval s >>= display
runFlat EmptyLine = display "\n"
runFlat (Label _) = pure ()
runFlat (Simple simple) = runSimple simple

data OptAttrs = OptAttrs
  { vis :: Bool
  , sel :: Bool
  }

applyIfMod :: MonadRun m => IfMod # Ann Loc -> OptAttrs -> m OptAttrs
applyIfMod (IfMod e) attrs = do
  v <- runTopEval e
  b <- valAs SBoolTy v
  pure $ attrs{vis = b && vis attrs}
applyIfMod (SelectableIfMod e) attrs = do
  v <- runTopEval e
  b <- valAs SBoolTy v
  pure $ attrs{sel = b && sel attrs}

applyReuseMod :: MonadRun m => Int -> ReuseMod # Ann Loc -> OptAttrs -> m OptAttrs
applyReuseMod i HideReuseMod attrs = do
  b <- isReused i
  pure $ attrs{vis = not b && vis attrs}
applyReuseMod _ AllowReuseMod attrs =
  pure $ attrs{vis = True}
applyReuseMod i DisableReuseMod attrs = do
  b <- isReused i
  pure $ attrs{sel = not b && sel attrs}

applyMods :: MonadRun m => Option (Const Pos) # Ann Loc -> m OptAttrs
applyMods opt = do
  ghr <- getHideReuse
  afterReuseMods <- foldlM (withCaret . flip (applyReuseMod $ optionId opt)) (OptAttrs ghr True) $ reuseMods opt
  foldlM (withCaret . flip applyIfMod) afterReuseMods $ ifMods opt

data FrontOpt tag (sel :: Bool) = MkFrontOpt Pos ByteString

toFrontOpt :: MonadRun m => Option (Const Pos) # Ann Loc -> m (Maybe (Some1 (FrontOpt tag)))
toFrontOpt opt = do
  attrs <- applyMods opt
  if vis attrs
    then do
      txt <- runTopPureEval $ optionText opt
      pure . Just $ case toSing (sel attrs) of
        SomeSing selSing -> Some1 selSing $ MkFrontOpt (getConst $ optionBody opt) txt
    else pure Nothing

runChoice :: MonadRun m => Choice (Const Pos) # Ann Loc -> m ()
runChoice (Choice _ opts) = do
  frontOpts <- catMaybes . NE.toList <$> mapM (withCaret toFrontOpt) opts
  MkFrontOpt pos _ <- choose frontOpts
  jumpToPos pos

runTarget :: MonadRun m => (Some1 Val -> m a) -> Target a # Ann Loc -> m a
runTarget _ (DirectTarget x) = pure x
runTarget f (RefTarget e) = runTopEval e >>= f

runTargetLabel :: MonadRun m => Target Label # Ann Loc -> m Label
runTargetLabel = runTarget $ fmap L . valAs SStrTy

runTargetVar :: MonadRun m => Target Var # Ann Loc -> m Var
runTargetVar = runTarget $ fmap V . valAs SStrTy

getFinishStr :: MonadRun m => Maybe (Str # Ann Loc) -> m ByteString
getFinishStr (Just str) = runTopPureEval str
getFinishStr Nothing = pure "Next Chapter"

getPageBreakStr :: MonadRun m => Maybe (Str # Ann Loc) -> m ByteString
getPageBreakStr (Just str) = runTopPureEval str
getPageBreakStr Nothing = pure "Next"

class RunSimple simple where
  runSimple :: MonadRun m => simple # Ann Loc -> m ()

instance RunSimple SimpleCommand where
  runSimple HideReuse = setHideReuse
  runSimple (Temp var e) = runTopEval e >>= setVar var
  runSimple (Set target se) = runTargetVar target >>= undefined
  runSimple (Delete var) = deleteVar var
  runSimple (InputNumber var lo hi) = inputNumber lo hi >>= setVar var . some1 . Val @IntTy
  runSimple (InputText var) = inputText >>= setVar var . some1 . Val @StrTy
  runSimple (Print var) = getVar var >>= \(Some1 t v) -> display $ printVal t v
  runSimple (Rand{}) = undefined
  runSimple (Goto target) = runTargetLabel target >>= jumpToLabel
  runSimple (GotoScene{}) = undefined
  runSimple (Gosub (SubArgs target args)) = do
    pushFrame
    lbl <- runTargetLabel target
    jumpToLabel lbl
    forM_ (zip [1 :: Int ..] args) $ \(i, e) -> do
      val <- runTopEval e
      setVar (V $ "param_" <> C.pack (show i)) val
  runSimple (GosubScene{}) = undefined
  runSimple (Params vars) =
    forM_ (zip [1 :: Int ..] $ NE.toList vars) $ \(i, var) ->
      getVar (V $ "param_" <> C.pack (show i)) >>= setVar var
  runSimple Return = popFrame *> nextLine
  runSimple (GotoRandomScene{}) = undefined
  runSimple (Finish fStr) = getFinishStr fStr >>= button
  runSimple LineBreak = display "\n"
  runSimple (PageBreak pbStr) = getPageBreakStr pbStr >>= button
  runSimple (Link{}) = undefined
  runSimple (StatChart{}) = undefined
  runSimple (Achieve{}) = undefined
  runSimple CheckAchievements = undefined
  runSimple Ending = undefined
