{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Runner (
  module Runner,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool.Singletons
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.Eq.Singletons
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.String.Singletons
import Exinst
import Exinst.Base ()
import Hyper
import Hyper.Recurse
import Text.Show.Singletons

import AST
import Compiler ()
import Lexer (Loc (..), LocType, Span)

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
  = VarNotFound
  | TypeMismatch ValTy [ValTy]
  | OutOfBounds Int
  deriving (Show)

data LocedEvalError = LEE EvalError Span
  deriving (Show)

data EvalCtx = EvalCtx {ctxLoc :: Span, ctxVars :: Vars}

type family EvaledType (h :: HyperType) where
  EvaledType Expr = Some1 Val
  EvaledType Str = ByteString
  EvaledType StrPart = ByteString
  EvaledType Multirep = ByteString

newtype Evaled h = Evaled {getEvaled :: EvaledType (GetHyperType h)}

throwEvalError :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => EvalError -> m a
throwEvalError e = asks (LEE e . ctxLoc) >>= throwError

class Eval h where
  eval :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => h # Evaled -> m (EvaledType h)
  default eval :: (Monad m, PureEval h) => h # Evaled -> m (EvaledType h)
  eval = pure . pureEval

class PureEval h where
  pureEval :: h # Evaled -> EvaledType h

prepareSpanned :: (MonadReader EvalCtx m, LocType h ~ Span) => (h # Ann Loc -> m a) -> Ann Loc # h -> m a
prepareSpanned f (Ann (Loc loc) x) = local (\ctx -> ctx{ctxLoc = loc}) $ f x

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

class Prepare h where
  prepare :: MonadReader EvalCtx m => (h # Ann Loc -> m a) -> Ann Loc # h -> m a
  prepare = (. _hVal)

instance Prepare Expr where
  prepare = prepareSpanned

instance Prepare Str

instance Prepare StrPart

instance Prepare Multirep where
  prepare = prepareSpanned

instance PureEval StrPart where
  pureEval sp = case sp of
    Normal s -> s
    SPMultirep (Evaled s) -> s
    Inter capMode (Evaled val) -> capitalize capMode $ withSome1Sing val printVal

instance Eval StrPart

multirepVal :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => Sing t -> Val t -> m Int
multirepVal t (Val v) = case t of
  SIntTy -> pure $ v - 1
  SBoolTy -> pure $ if v then 0 else 1
  SStrTy -> throwEvalError $ TypeMismatch StrTy [IntTy, BoolTy]

instance Eval Multirep where
  eval (Multirep (Evaled val) ss) = do
    i <- withSome1Sing val multirepVal
    if 0 <= i && i < length ss
      then pure . getEvaled $ ss NE.!! i
      else throwEvalError $ OutOfBounds i

instance PureEval Str where
  pureEval (S sps) = B.concat $ map pureEval sps

instance Eval Str

evalVar :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => Var -> m (Some1 Val)
evalVar var =
  asks ((HM.!? var) . ctxVars) >>= \case
    Just val -> pure val
    Nothing -> throwEvalError VarNotFound

valAs :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => Sing t -> Some1 Val -> m (TypeOf t)
valAs t (Some1 t' val) = case t %~ t' of
  Proved Refl -> pure $ getVal val
  Disproved _ -> throwEvalError $ TypeMismatch (fromSing t') [fromSing t]

int :: Int -> Val IntTy
int = Val

bool :: Bool -> Val BoolTy
bool = Val

str :: ByteString -> Val StrTy
str = Val

evalUnOp :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => UnOp -> Some1 Val -> m (Some1 Val)
evalUnOp Neg = fmap (someVal SIntTy) . apply SIntTy negate
evalUnOp Not = fmap (someVal SBoolTy) . apply SBoolTy not

apply :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => Sing t1 -> (TypeOf t1 -> x) -> Some1 Val -> m x
apply t1 f x = f <$> valAs t1 x

apply2
  :: (MonadError LocedEvalError m, MonadReader EvalCtx m)
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

arith :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => (Int -> Int -> Int) -> Some1 Val -> Some1 Val -> m (Some1 Val)
arith = fmap (someVal SIntTy) .:. apply2 SIntTy SIntTy

cmp :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => (Int -> Int -> Bool) -> Some1 Val -> Some1 Val -> m (Some1 Val)
cmp = fmap (someVal SBoolTy) .:. apply2 SIntTy SIntTy

logic :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => (Bool -> Bool -> Bool) -> Some1 Val -> Some1 Val -> m (Some1 Val)
logic = fmap (someVal SBoolTy) .:. apply2 SBoolTy SBoolTy

evalBinOp :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => BinOp -> Some1 Val -> Some1 Val -> m (Some1 Val)
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
    _ -> throwEvalError $ TypeMismatch (fromSing t2) [fromSing t1]
evalBinOp Neq = \x y -> evalBinOp Eq x y >>= evalUnOp Not
evalBinOp Gt = cmp (>)
evalBinOp Lt = cmp (<)
evalBinOp And = logic (&&)
evalBinOp Or = logic (||)
evalBinOp Get = \(Some1 t1 (Val x)) (Some1 t2 (Val y)) ->
  some1 . str <$> case (t1, t2) of
    (SStrTy, SIntTy) -> case x C.!? (y - 1) of
      Just c -> pure $ C.singleton c
      Nothing -> throwEvalError $ OutOfBounds y
    (SStrTy, _) -> throwEvalError $ TypeMismatch (fromSing t2) [IntTy]
    _ -> throwEvalError $ TypeMismatch (fromSing t1) [StrTy]
evalBinOp Cat = fmap (someVal SStrTy) .: apply2 SStrTy SStrTy (<>)

evalFun :: (MonadError LocedEvalError m, MonadReader EvalCtx m) => Fun -> Some1 Val -> m (Some1 Val)
evalFun Length = fmap (someVal SIntTy) . apply SStrTy C.length

instance Eval Expr where
  eval (Constant (Int n)) = pure $ some1 $ Val @IntTy n
  eval (Constant (Bool b)) = pure $ some1 $ Val @BoolTy b
  eval (Str (Evaled s)) = pure $ some1 $ Val @StrTy s
  eval (Var (DirectTarget var)) = evalVar var
  eval (Var (RefTarget (Evaled val))) = valAs SStrTy val >>= evalVar . V
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

class (RTraversable h, Recursively Eval h, Recursively Prepare h) => TopLoc h where
  topLoc :: Vars -> Ann Loc # h -> EvalCtx

instance (RTraversable h, Recursively Eval h, Recursively Prepare h, LocType h ~ Span) => TopLoc h where
  topLoc vars (Ann (Loc loc) _) = EvalCtx loc vars

evalTop :: (MonadError LocedEvalError m, TopLoc h) => Vars -> Ann Loc # h -> m (EvaledType h)
evalTop vars e = getEvaled <$> runReaderT (hthing (Proxy @Eval ##>> fmap Evaled . eval) (Proxy @Prepare ##>> prepare) e) (topLoc vars e)

pureTop :: (MonadError LocedEvalError m, PureEval h, HTraversable h, HNodesConstraint h TopLoc) => Vars -> h # Ann Loc -> m (EvaledType h)
pureTop vars = fmap pureEval . htraverse (Proxy @TopLoc #> fmap Evaled . evalTop vars)

exprTopWitness :: Dict (TopLoc Expr)
exprTopWitness = Dict

strPureWitness :: Dict (PureEval Str, HNodesConstraint Str TopLoc)
strPureWitness = Dict

data ProgState = ProgState
  { vars :: Vars
  , pos :: Int
  }