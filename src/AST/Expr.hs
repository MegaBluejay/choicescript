{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Expr where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Hyper

import AST.TH

data Expr h
  = Constant Constant
  | Str (h :# Str)
  | Var Var
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
  | Multirep (h :# Expr) (NonEmpty (h :# Str))
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
  = Round
  | Length
  deriving (Show, Generic)

newtype Var = V {getV :: ByteString}
  deriving (Show, Generic)

makeAll [''StrPart, ''Str, ''Expr]
