{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Runner (
  module Runner,
) where

import Control.Lens hiding (Choice)
import Control.Lens.Internal.Zoom
import Control.Lens.Unsound
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool.Singletons
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.Eq.Singletons
import Data.Foldable
import Data.Function
import Data.Generics.Product.Fields
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Singletons.Decide qualified as Decide
import Data.Singletons.TH hiding ((%~))
import Data.String.Singletons
import Data.Vector (Vector)
import Exinst
import Exinst.Base ()
import Hyper
import Hyper.Recurse
import Text.Regex.Lens
import Text.Regex.PCRE
import Text.Regex.Quote
import Text.Show.Singletons

import AST
import Compiler ()
import Data.IntSet (IntSet)
import Lexer (Caret, Careted (..), Loc (..), LocType, Span, Spanned (..))
import Parser ()

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
valAs t (Some1 t' val) = case t Decide.%~ t' of
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

newtype GlobalRunState = GlobalRunState {globalVars :: Vars}
  deriving (Generic, Show)

data RunState = RunState
  { globalCtx :: GlobalRunState
  , tempVars :: Vars
  , reusedOpts :: IntSet
  , runSettings :: RunSettings
  , runPos :: Pos
  , stack :: [RunFrame]
  }
  deriving (Generic, Show)

data RunFrame = RunFrame
  { args :: [Some1 Val]
  , returnPos :: Pos
  }
  deriving (Generic, Show)

data RunSettings = RunSettings
  { hideReuse :: Bool
  , implicitFlow :: Bool
  }
  deriving (Generic, Show)

data RunCtx simple = RunCtx
  { lbls :: HashMap Label Pos
  , prog :: Vector (CLine simple # Ann Loc)
  , runLoc :: Caret
  }
  deriving (Generic)

data RunError
  = RunVarNotFound Var
  | ParamNotFound Int
  | NoFrame
  | LabelNotFound Label
  | VarExists Var
  | RunTypeMismatch ValTy [ValTy]
  | NoImplicitFlow
  | InvalidParam ByteString
  | ParamsLengthMismatch Int Int
  | EvalError (Spanned EvalError)
  deriving (Generic, Show)

type ValPair = (Maybe (Some1 Val), Maybe (Some1 Val))

varsPair :: Lens' RunState (Vars, Vars)
varsPair = lensProduct (field @"globalCtx" . field @"globalVars") (field @"tempVars")

valPair :: Var -> Lens' RunState ValPair
valPair var = lensProduct (varsPair . _1 . at var) (varsPair . _2 . at var)

getVar' :: (MonadRunError m, MonadReader ValPair m) => Var -> m (Some1 Val)
getVar' var = do
  (gval, tval) <- ask
  case gval of
    Just val -> pure val
    Nothing -> case tval of
      Just val -> pure val
      Nothing -> throwRunError $ RunVarNotFound var

setVar' :: (MonadRunError m, MonadState ValPair m) => Var -> Some1 Val -> m ()
setVar' var val = do
  (gval, tval) <- get
  case gval of
    Just _ -> _1 .= Just val
    Nothing -> case tval of
      Just _ -> _2 .= Just val
      Nothing -> throwRunError $ RunVarNotFound var

tempVar' :: (MonadRunError m, MonadState ValPair m) => Var -> Some1 Val -> m ()
tempVar' var val =
  get >>= \case
    (Nothing, Nothing) -> _2 .= Just val
    _ -> throwRunError $ VarExists var

createVar' :: (MonadRunError m, MonadState ValPair m) => Var -> Some1 Val -> m ()
createVar' var val =
  get >>= \case
    (Nothing, Nothing) -> _1 .= Just val
    _ -> throwRunError $ VarExists var

deleteVar' :: (MonadRunError m, MonadState ValPair m) => Var -> m ()
deleteVar' var = do
  (gval, tval) <- get
  case gval of
    Just _ -> _1 .= Nothing
    Nothing -> case tval of
      Just _ -> _2 .= Nothing
      Nothing -> throwRunError $ RunVarNotFound var

varParam :: MonadRunError m => Var -> m (Maybe Int)
varParam (V v) = case v ^? regex [r|^param_([1-9]\d*)?|] . captures . ix 0 of
  Just "" -> throwRunError $ InvalidParam v
  Just p -> pure . Just . read $ C.unpack p
  Nothing -> pure Nothing

newtype RunT r s e m a = RunT {unRunT :: ReaderT r (StateT s (ExceptT e m)) a}
  deriving newtype (Functor, Applicative, Monad, MonadState s, MonadError e, MonadReader r)

runRunT :: Monad m => RunT r s e m a -> r -> s -> m (Either e a)
runRunT x r s = runExceptT $ evalStateT (runReaderT (unRunT x) r) s

instance MonadTrans (RunT r s e) where
  lift = RunT . lift . lift . lift

type instance Zoomed (RunT r s e m) = Focusing (ExceptT e m)

instance Monad m => Zoom (RunT r s e m) (RunT r t e m) s t where
  zoom l (RunT x) = RunT $ zoom l x

type RunT' simple = RunT (RunCtx simple) RunState (Careted RunError)

class (MonadInter m, MonadRunError m, MonadCast m) => MonadRun m where
  getVar :: Var -> m (Some1 Val)
  setVar :: Var -> Some1 Val -> m ()
  tempVar :: Var -> Some1 Val -> m ()
  deleteVar :: Var -> m ()

  nextLine :: m ()
  jumpToPos :: Pos -> m ()
  jumpToLabel :: Label -> m ()

  withCaret :: LocType h ~ Caret => (h # Ann Loc -> m a) -> Ann Loc # h -> m a

  runTopEval :: Recursively Evalable h => Ann Loc # h -> m (EvaledType h)
  runTopPureEval :: PureEvalable h => h # Ann Loc -> m (EvaledType h)

  withSettings :: State RunSettings a -> m a

  checkReused :: Int -> m Bool

  pushFrame :: RunFrame -> m ()
  popFrame :: m Pos

  getPos :: m Pos

  getArgs :: m [Some1 Val]

class MonadRun m => MonadStartup m where
  createVar :: Var -> Some1 Val -> m ()

class Monad m => MonadRunError m where
  throwRunError :: RunError -> m a

instance Monad m => MonadRunError (RunT (RunCtx simple) s (Careted RunError) m) where
  throwRunError e = view (field @"runLoc") >>= throwError . (e :^)

evalToRun :: MonadRunError m => Either (Spanned EvalError) a -> m a
evalToRun = either (throwRunError . EvalError) pure

unifiedVars :: Getter RunState Vars
unifiedVars = varsPair . to (uncurry HM.union)

instance Monad m => MonadCast (RunT (RunCtx simple) s (Careted RunError) m) where
  throwCastError got exp = throwRunError $ RunTypeMismatch got [exp]

class Monad m => MonadInter m where
  display :: ByteString -> m ()
  button :: ByteString -> m ()
  choose :: [Some1 (FrontOpt tag)] -> m (FrontOpt tag True)
  inputNumber :: Int -> Int -> m Int
  inputText :: m ByteString

instance MonadInter m => MonadInter (RunT r s e m) where
  display = lift . display
  button = lift . button
  choose = lift . choose
  inputNumber = lift .: inputNumber
  inputText = lift inputText

instance MonadInter m => MonadRun (RunT' simple m) where
  getVar var =
    varParam var >>= \case
      Just i ->
        preuse (field @"stack" . ix 0 . field @"args" . ix i) >>= \case
          Just val -> pure val
          Nothing -> throwRunError $ ParamNotFound i
      Nothing -> zoom (valPair var) $ getRO $ getVar' var

  setVar var val = zoom (valPair var) $ setVar' var val
  tempVar var val = zoom (valPair var) $ tempVar' var val
  deleteVar var = zoom (valPair var) $ deleteVar' var

  nextLine = field @"runPos" . _Wrapped' %= succ
  jumpToPos pos = field @"runPos" .= pos
  jumpToLabel lbl =
    view (field @"lbls" . at lbl) >>= \case
      Just pos -> jumpToPos pos
      Nothing -> throwRunError $ LabelNotFound lbl

  withCaret f (Ann (Loc loc) x) = local (field @"runLoc" .~ loc) $ f x

  runTopEval x = use unifiedVars >>= evalToRun . flip topEval x
  runTopPureEval x = use unifiedVars >>= evalToRun . flip topPureEval x

  withSettings = zoom (field @"runSettings") . state . runState

  checkReused i = field @"reusedOpts" . at i %%= \x -> (isJust x, Just ())

  pushFrame frame = field @"stack" %= (frame :)
  popFrame =
    zoom (field @"stack") $
      gets uncons >>= \case
        Just (f, fs) -> returnPos f <$ put fs
        Nothing -> throwRunError NoFrame

  getPos = use $ field @"runPos"

  getArgs =
    preuse (field @"stack" . ix 0 . field @"args") >>= \case
      Just as -> pure as
      Nothing -> throwRunError NoFrame

newtype RO m a = RO {getRO :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadRunError)

instance MonadState s m => MonadReader s (RO m) where
  ask = RO get
  local f (RO x) = RO $ do
    st <- get
    put $ f st
    res <- x
    put st
    pure res

data FrontOpt tag (sel :: Bool) = MkFrontOpt Pos ByteString

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
  b <- checkReused i
  pure $ attrs{vis = not b && vis attrs}
applyReuseMod _ AllowReuseMod attrs =
  pure $ attrs{vis = True}
applyReuseMod i DisableReuseMod attrs = do
  b <- checkReused i
  pure $ attrs{sel = not b && sel attrs}

applyMods :: MonadRun m => Option (Const Pos) # Ann Loc -> m OptAttrs
applyMods opt = do
  ghr <- withSettings $ use $ field @"hideReuse"
  afterReuseMods <- foldlM (withCaret . flip (applyReuseMod $ optionId opt)) (OptAttrs ghr True) $ reuseMods opt
  foldlM (withCaret . flip applyIfMod) afterReuseMods $ ifMods opt

toFrontOpt :: MonadRun m => Option (Const Pos) # Ann Loc -> m (Maybe (Some1 (FrontOpt tag)))
toFrontOpt opt = do
  attrs <- applyMods opt
  if vis attrs
    then do
      txt <- runTopPureEval $ optionText opt
      pure . Just $ case toSing (sel attrs) of
        SomeSing selSing -> Some1 selSing $ MkFrontOpt (getConst $ optionBody opt) txt
    else pure Nothing

runLine :: (RunSimple simple, MonadRun m) => CLine simple # Ann Loc -> m ()
runLine (CLoc cLoc) = withCaret runCLoc cLoc
runLine (ImplicitJumpIf pos) = jumpToPos pos
runLine (ImplicitJumpChoice cm pos) = case cm of
  FakeChoiceMode -> jumpToPos pos
  ChoiceMode -> do
    imp <- withSettings $ use $ field @"implicitFlow"
    if imp then jumpToPos pos else throwRunError NoImplicitFlow

runCLoc :: (RunSimple simple, MonadRun m) => CLoc simple # Ann Loc -> m ()
runCLoc (CFlat flat) = runFlat flat
runCLoc (JumpUnless e pos) = do
  v <- runTopEval e
  b <- valAs SBoolTy v
  if b then nextLine else jumpToPos pos
runCLoc (CChoice choice) = runChoice choice

runFlat :: (RunSimple simple, MonadRun m) => FlatLine simple # Ann Loc -> m ()
runFlat (Text s) = runTopPureEval s >>= display
runFlat EmptyLine = display "\n"
runFlat (Label{}) = pure ()
runFlat (Simple simple) = runSimple simple

runChoice :: MonadRun m => Choice (Const Pos) # Ann Loc -> m ()
runChoice (Choice _ opts) = do
  frontOpts <- catMaybes . NE.toList <$> mapM (withCaret toFrontOpt) opts
  MkFrontOpt pos _ <- choose frontOpts
  jumpToPos pos

class RunSimple simple where
  runSimple :: MonadRun m => simple # Ann Loc -> m ()

runTargetLabel :: MonadRun m => Target Label # Ann Loc -> m Label
runTargetLabel (DirectTarget lbl) = pure lbl
runTargetLabel (RefTarget e) = do
  v <- runTopEval e
  s <- valAs SStrTy v
  pure $ L s

finishText :: MonadRun m => Maybe (Str # Ann Loc) -> m ByteString
finishText (Just s) = runTopPureEval s
finishText Nothing = pure "Next Chapter"

pageBreakText :: MonadRun m => Maybe (Str # Ann Loc) -> m ByteString
pageBreakText (Just s) = runTopPureEval s
pageBreakText Nothing = pure "Next"

instance RunSimple SimpleCommand where
  runSimple HideReuse = withSettings $ field @"hideReuse" .= True
  runSimple (Temp var e) = runTopEval e >>= tempVar var
  runSimple (Delete var) = deleteVar var
  runSimple (InputNumber var lo hi) = inputNumber lo hi >>= setVar var . some1 . Val @IntTy
  runSimple (InputText var) = inputText >>= setVar var . some1 . Val @StrTy
  runSimple (Print var) = getVar var >>= \(Some1 t v) -> display $ printVal t v
  runSimple (Goto target) = runTargetLabel target >>= jumpToLabel
  runSimple (Gosub (SubArgs target args)) = do
    lbl <- runTargetLabel target
    argVals <- mapM runTopEval args
    cpos <- getPos
    pushFrame $ RunFrame argVals (cpos + 1)
    jumpToLabel lbl
    forM_ (zip [1 :: Int ..] args) $ \(i, e) ->
      runTopEval e >>= setVar (V $ "param_" <> C.pack (show i))
  runSimple (Params vars) = do
    as <- getArgs
    case (length as, NE.length vars) of
      (al, vl)
        | al == vl -> forM_ (zip (NE.toList vars) as) $ uncurry setVar
        | otherwise -> throwRunError $ ParamsLengthMismatch al vl
  runSimple Return = popFrame >>= jumpToPos
  runSimple LineBreak = display "\n"
  runSimple (Finish text) = finishText text >>= button
  runSimple (PageBreak text) = pageBreakText text >>= button
  runSimple _ = undefined
