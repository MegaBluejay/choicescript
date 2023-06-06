{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Eval (module Eval) where

import Control.Lens hiding (Index, op)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Hyper
import Hyper.Recurse

import AST.Expr

class Monad m => MonadCast m where
  throwTypeError :: TypeError -> m a

asInt :: MonadCast m => Val -> m Int
asInt (IntVal n) = pure n
asInt (BoolVal _) = throwTypeError $ TypeError [IntTy] BoolTy
asInt (StrVal _) = throwTypeError $ TypeError [IntTy] StrTy

asBool :: MonadCast m => Val -> m Bool
asBool (BoolVal b) = pure b
asBool (IntVal _) = throwTypeError $ TypeError [BoolTy] IntTy
asBool (StrVal _) = throwTypeError $ TypeError [BoolTy] StrTy

asStr :: MonadCast m => Val -> m ByteString
asStr (StrVal s) = pure s
asStr (IntVal _) = throwTypeError $ TypeError [StrTy] IntTy
asStr (BoolVal _) = throwTypeError $ TypeError [StrTy] BoolTy

class MBounded t where
  type Item t
  type Index t
  getBounded :: t -> Index t -> Either (Item t) OutOfBounds

instance MBounded ByteString where
  type Item ByteString = Char
  type Index ByteString = Int
  getBounded s i = case s C.!? i of
    Just c -> Left c
    Nothing -> Right $ OutOfBounds (C.length s) i

instance MBounded (NonEmpty a) where
  type Item (NonEmpty a) = a
  type Index (NonEmpty a) = Int

  getBounded l i =
    if 0 <= i && i < NE.length l
      then Left $ l NE.!! i
      else Right $ OutOfBounds (NE.length l) i

class Monad m => MonadBounded m where
  checkBounds :: MBounded t => t -> Index t -> m (Item t)

class (MonadCast m, MonadBounded m) => MonadEval m where
  throwEvalError :: EvalError -> m a
  getVar :: Var -> m Val

type family EvaledType (h :: HyperType) where
  EvaledType Expr = Val
  EvaledType Multirep = ByteString

newtype Evaled (h :: AHyperType) = Evaled {getEvaled :: EvaledType (GetHyperType h)}

class Evalable h where
  eval :: MonadEval m => h # Evaled -> m (EvaledType h)

data ValTy = IntTy | BoolTy | StrTy
  deriving (Show)

data Val
  = IntVal Int
  | BoolVal Bool
  | StrVal ByteString
  deriving (Show)

data TypeError = TypeError
  { _expected :: [ValTy]
  , _got :: ValTy
  }
  deriving (Show)

data OutOfBounds = OutOfBounds
  { _maxAllowed :: Int
  , _got :: Int
  }
  deriving (Show)

data EvalError
  = EvalVarNotFound Var
  | EvalTypeError TypeError
  | EvalOutOfBounds OutOfBounds
  deriving (Show)

printVal :: Val -> ByteString
printVal (IntVal n) = C.pack $ show n
printVal (BoolVal b) = if b then "true" else "false"
printVal (StrVal s) = s

capitalize :: CapMode -> ByteString -> ByteString
capitalize NoCap s = s
capitalize Cap s = case C.uncons s of
  Just (x, xs) -> C.cons (toUpper x) xs
  Nothing -> ""
capitalize Upper s = C.map toUpper s

evalStrPart :: StrPart # Evaled -> ByteString
evalStrPart (Normal s) = s
evalStrPart (Inter capMode (Evaled val)) = capitalize capMode $ printVal val
evalStrPart (SPMultirep (Evaled s)) = s

evalStr :: Str # Evaled -> ByteString
evalStr (S ss) = B.concat $ map evalStrPart ss

intUnOp :: MonadCast m => (Int -> Int) -> Val -> m Val
intUnOp f a = IntVal . f <$> asInt a

boolUnOp :: MonadCast m => (Bool -> Bool) -> Val -> m Val
boolUnOp f a = BoolVal . f <$> asBool a

evalUnOp :: MonadCast m => UnOp -> Val -> m Val
evalUnOp Neg = intUnOp negate
evalUnOp Not = boolUnOp not

intBinOp :: MonadCast m => (Int -> Int -> Int) -> Val -> Val -> m Val
intBinOp f a b = do
  a' <- asInt a
  b' <- asInt b
  pure . IntVal $ f a' b'

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

cmp :: MonadCast m => (Int -> Int -> Bool) -> Val -> Val -> m Val
cmp f a b = do
  a' <- asInt a
  b' <- asInt b
  pure . BoolVal $ f a' b'

logic :: MonadCast m => (Bool -> Bool -> Bool) -> Val -> Val -> m Val
logic f a b = do
  a' <- asBool a
  b' <- asBool b
  pure . BoolVal $ f a' b'

evalBinOp :: (MonadCast m, MonadBounded m) => BinOp -> Val -> Val -> m Val
evalBinOp Add = intBinOp (+)
evalBinOp Sub = intBinOp (-)
evalBinOp Mul = intBinOp (*)
evalBinOp Div = intBinOp (round @Double @Int .: (/) `on` fromIntegral)
evalBinOp Mod = intBinOp mod
evalBinOp FAdd = intBinOp $ \x y ->
  let x' = fromIntegral x
      y' = fromIntegral y
   in round @Double @Int $ x' + (100 - x') * y' / 100
evalBinOp FSub = intBinOp $ \x y ->
  let x' = fromIntegral x
      y' = fromIntegral y
   in round @Double @Int $ x' - x' * y' / 100
evalBinOp Eq =
  fmap BoolVal .: \case
    IntVal n -> fmap (n ==) . asInt
    BoolVal b -> fmap (b ==) . asBool
    StrVal s -> fmap (s ==) . asStr
evalBinOp Neq = (>>= evalUnOp Not) .: evalBinOp Eq
evalBinOp Gt = cmp (>)
evalBinOp Lt = cmp (<)
evalBinOp And = logic (&&)
evalBinOp Or = logic (||)
evalBinOp Get = \s i -> do
  s' <- asStr s
  i' <- asInt i
  c <- checkBounds s' i'
  pure . StrVal $ C.singleton c
evalBinOp Cat = \s1 s2 -> do
  s1' <- asStr s1
  s2' <- asStr s2
  pure . StrVal $ s1' <> s2'

evalFun :: MonadCast m => Fun -> Val -> m Val
evalFun Length = fmap (IntVal . C.length) . asStr

instance Evalable Expr where
  eval (Constant (Int n)) = pure $ IntVal n
  eval (Constant (Bool n)) = pure $ BoolVal n
  eval (Str s) = pure . StrVal $ evalStr s
  eval (Var (DirectTarget var)) = getVar var
  eval (Var (RefTarget (Evaled val))) = asStr val >>= getVar . V
  eval (UnOp op (Evaled a)) = evalUnOp op a
  eval (BinOp (Evaled a) op (Evaled b)) = evalBinOp op a b
  eval (Fun fun (Evaled a)) = evalFun fun a

instance Evalable Multirep where
  eval (Multirep (Evaled val) ss) = fmap evalStr $ case val of
    IntVal n -> checkBounds ss n
    BoolVal b -> checkBounds ss (if b then 0 else 1)
    StrVal _ -> throwTypeError $ TypeError [IntTy, BoolTy] StrTy

hpara
  :: forall h w0 w1 m
   . (RTraversable h, Monad m)
  => (forall n. HRecWitness h n -> w0 # n -> n # w1 -> m (w1 # n))
  -> (forall n. HRecWitness h n -> w0 # n -> n # w0)
  -> w0 # h
  -> m (w1 # h)
hpara f g x =
  withDict (recursiveHTraversable $ Proxy @h) $
    f HRecSelf x =<< htraverse (Proxy @RTraversable #*# \w -> hpara (f . HRecSub w) (g . HRecSub w)) (g HRecSelf x)

withAnn
  :: (RTraversable h, Monad m)
  => (forall n. HRecWitness h n -> a -> n # w1 -> m (w1 # n))
  -> Annotated a # h
  -> m (w1 # h)
withAnn f = hpara (\w (Ann (Const a) _) -> f w a) (const $ view hVal)
