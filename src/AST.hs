{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST (
  module AST,
  module AST.Expr,
) where

import Data.ByteString (ByteString)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Hyper
import Hyper.Class.Recursive

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

data SetExpr h
  = NormalSet (h :# Expr)
  | ModSet BinOp (h :# Expr)
  deriving (Generic)

newtype Label = L {getL :: ByteString}
  deriving (Show, Eq, Hashable, Generic)

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
  , ''SetExpr
  , ''CLoc
  , ''CLine
  ]

instance RNodes simple => RNodes (FlatLine simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (FlatLine simple), c StrPart, c Str, c Expr, Recursively c simple) => Recursively c (FlatLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict

instance RNodes simple => RNodes (PLine simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (PLine simple), c (If simple), c (Else simple), c (PBody simple), c (Option (PBody simple)), c OptionMod, c StrPart, c Str, c Expr, Recursively c simple) => Recursively c (PLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict

instance RNodes simple => RNodes (PBody simple)
instance (c (PBody simple), c (PLine simple), c (If simple), c (Else simple), c (Option (PBody simple)), c OptionMod, c StrPart, c Str, c Expr, Recursively c simple) => Recursively c (PBody simple)

instance RNodes simple => RNodes (If simple)
instance (c (If simple), c (PLine simple), c (Else simple), c (PBody simple), c (Option (PBody simple)), c OptionMod, c StrPart, c Str, c Expr, Recursively c simple) => Recursively c (If simple)

instance RNodes simple => RNodes (Else simple)
instance (c (Else simple), c (PLine simple), c (If simple), c (PBody simple), c (Option (PBody simple)), c OptionMod, c StrPart, c Str, c Expr, Recursively c simple) => Recursively c (Else simple)

instance RNodes simple => RNodes (CLoc simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (CLoc simple), c (Option (Const Int)), c OptionMod, c StrPart, c Str, c Expr, c (Const Int), Recursively c simple) => Recursively c (CLoc simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict

instance RNodes simple => RNodes (CLine simple)
instance (c (CLine simple), c (CLoc simple), c (Option (Const Int)), c OptionMod, c StrPart, c Str, c Expr, c (Const Int), Recursively c simple) => Recursively c (CLine simple)

instance RNodes body => RNodes (Choice body)
instance (c (Choice body), c (Option body), c OptionMod, c StrPart, c Str, c Expr, Recursively c body) => Recursively c (Choice body)

instance RNodes body => RNodes (Option body) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (c (Option body), c OptionMod, c StrPart, c Str, c Expr, Recursively c body) => Recursively c (Option body) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c body)) Dict

instance RNodes OptionMod
instance (c OptionMod, c StrPart, c Str, c Expr) => Recursively c OptionMod

instance RNodes StartupSimpleCommand
instance (c StartupSimpleCommand, c StrPart, c Str, c Expr) => Recursively c StartupSimpleCommand

instance RNodes AchData
instance (c AchData, c StrPart, c Str, c Expr) => Recursively c AchData

instance RNodes SimpleCommand
instance (c SimpleCommand, c StrPart, c Str, c Expr) => Recursively c SimpleCommand

instance RNodes SubArgs
instance (c SubArgs, c StrPart, c Str, c Expr) => Recursively c SubArgs

instance RNodes SetExpr
instance (c SetExpr, c StrPart, c Str, c Expr) => Recursively c SetExpr

pBodyWitness :: Dict (Recursively HFunctor (PBody StartupSimpleCommand))
pBodyWitness = Dict

cLineWitness :: Dict (Recursively HFunctor (CLine StartupSimpleCommand))
cLineWitness = Dict
