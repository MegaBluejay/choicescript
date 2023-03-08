{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST (
  module AST,
  module AST.Expr,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Hyper

import AST.Expr
import AST.TH

data FlatLine simple h
  = Text (h :# Str)
  | EmptyLine
  | Label Label
  | Simple (simple h)
  deriving (Generic)

data PLine simple h
  = PFlat (FlatLine simple h)
  | PIf (If simple h)
  | PChoice (Choice (PBody simple) h)
  deriving (Generic)

newtype PBody simple h = PBody [h :# PLine simple]
  deriving (Generic)

data If simple h
  = If (h :# Expr) (PBody simple h) (Maybe (h :# Else simple))
  deriving (Generic)

data Else simple h
  = Elseif (If simple h)
  | Else (PBody simple h)
  deriving (Generic)

data CLoc simple h
  = CFlat (FlatLine simple h)
  | JumpUnless (h :# Expr) Int
  | CChoice (Choice (Const Int) h)
  deriving (Generic)

data CLine simple h
  = CLoc (h :# CLoc simple)
  | ImplicitJump Int
  deriving (Generic)

data Choice body h
  = Choice ChoiceMode (NonEmpty (h :# Option body))
  deriving (Generic)

data ChoiceMode = ChoiceMode | FakeChoiceMode
  deriving (Generic, Eq, Ord, Show)

data Option body h = Option
  { optionMods :: [h :# OptionMod]
  , optionText :: h :# Str
  , optionBody :: body h
  }
  deriving (Generic)

data OptionMod h
  = IfMod (h :# Expr)
  | SelectableIfMod (h :# Expr)
  | HideReuseMod
  | AllowReuseMod
  | DisableReuseMod
  deriving (Generic)

data StartupSimpleCommand h
  = Create Var (h :# Expr)
  | SceneList (NonEmpty SceneName)
  | Title (h :# Str)
  | Author (h :# Str)
  | Achievement Achievement (AchData h)
  | NormalSimple (SimpleCommand h)
  deriving (Generic)

data AchData h = AchData
  { visible :: Bool
  , points :: Int
  , title :: h :# Str
  , preDesc :: h :# Str
  , postDesc :: h :# Str
  }
  deriving (Generic)

data SimpleCommand h
  = HideReuse
  | Temp Var (h :# Expr)
  | Set (Target Var h) (SetExpr h)
  | Delete Var
  | InputNumber Var Int Int
  | InputText Var
  | Print Var
  | Rand Var Int Int
  | Goto (Target Label h)
  | GotoScene SceneName (Maybe (Target Label h))
  | Gosub (SubArgs h)
  | GosubScene SceneName (Maybe (SubArgs h))
  | Params (NonEmpty Var)
  | Return
  | GotoRandomScene (NonEmpty SceneName)
  | Finish (Maybe (h :# Str))
  | LineBreak
  | PageBreak (Maybe (h :# Str))
  | Link ByteString (Maybe (h :# Str))
  | StatChart (NonEmpty Stat)
  | Achieve Achievement
  | CheckAchievements
  | Ending
  deriving (Generic)

data SubArgs h = SubArgs (Target Label h) [h :# Expr]
  deriving (Generic)

data NamedStat
  = JustVar ByteString
  | Named Var ByteString
  deriving (Show, Generic)

data Stat
  = TextStat ByteString
  | PercentStat NamedStat
  | OpposedStat NamedStat ByteString
  deriving (Show, Generic)

data Target a h
  = DirectTarget a
  | RefTarget (h :# Expr)
  deriving (Generic)

data SetExpr h
  = NormalSet (h :# Expr)
  | ModSet BinOp (h :# Expr)
  deriving (Generic)

newtype Label = L {getL :: ByteString}
  deriving (Show, Generic)

newtype SceneName = SN {getSN :: ByteString}
  deriving (Show, Generic)

newtype Achievement = A {getA :: ByteString}
  deriving (Show, Generic)

makeAll
  [ ''FlatLine
  , ''PLine
  , ''PBody
  , ''If
  , ''Else
  , ''Choice
  , ''Option
  , ''OptionMod
  , ''StartupSimpleCommand
  , ''AchData
  , ''SimpleCommand
  , ''SubArgs
  , ''Target
  , ''SetExpr
  , ''CLoc
  , ''CLine
  ]

-- makeDerivings [''Show] [''If, ''Else, ''OptionMod, ''Option, ''Choice, ''FlatLine, ''ParsedBody, ''ParsedLine, ''CompiledLoc, ''CompiledLine]
