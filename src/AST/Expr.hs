{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Expr where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Hyper

import AST.TH

data Target a h
  = DirectTarget a
  | RefTarget (h :# Expr)
  deriving (Generic)

data Expr h
  = Constant Constant
  | Str (Str h)
  | Var (Target Var h)
  | UnOp UnOp (h :# Expr)
  | BinOp (h :# Expr) BinOp (h :# Expr)
  | Fun Fun (h :# Expr)
  deriving (Generic)

data Constant
  = Int Int
  | Bool Bool
  deriving (Show, Generic)

data StrPart h
  = Normal ByteString
  | Inter CapMode (h :# Expr)
  | SPMultirep (h :# Multirep)
  deriving (Generic)

data Multirep h = Multirep (h :# Expr) (NonEmpty (Str h))
  deriving (Generic)

newtype Str h = S [StrPart h]
  deriving (Generic)

data CapMode = Cap | Upper | NoCap
  deriving (Show, Generic)

data UnOp
  = Neg
  | Not
  deriving (Show, Generic)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | FAdd
  | FSub
  | Eq
  | Neq
  | Gt
  | Lt
  | And
  | Or
  | Get
  | Cat
  deriving (Show, Generic)

data Fun
  = Length
  deriving (Show, Generic)

newtype Var = V {getV :: ByteString}
  deriving (Show, Eq, Hashable, Generic)

instance Wrapped (Str h)
instance (t ~ [StrPart h]) => Rewrapped (Str h) t

instance Wrapped Var
instance (t ~ ByteString) => Rewrapped Var t

makeAll [''Target, ''StrPart, ''Str, ''Expr, ''Multirep]

instance RNodes (Target a)
instance (c (Target a), c Expr, c Multirep) => Recursively c (Target a)
instance RTraversable (Target a)

instance RNodes Multirep
instance (c Expr, c Multirep) => Recursively c Multirep
instance RTraversable Multirep

instance RNodes StrPart
instance (c StrPart, c Expr, c Multirep) => Recursively c StrPart
instance RTraversable StrPart

instance RNodes Str
instance (c Str, c Expr, c Multirep) => Recursively c Str
instance RTraversable Str

instance RNodes Expr
instance (c Expr, c Multirep) => Recursively c Expr
instance RTraversable Expr
