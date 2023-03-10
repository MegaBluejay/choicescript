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

instance RNodes simple => RNodes (FlatLine simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (FlatLine simple), Recursively c Str, Recursively c simple) => Recursively c (FlatLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict

instance RNodes simple => RNodes (PLine simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (PLine simple), Recursively c (FlatLine simple), Recursively c (If simple), Recursively c (Choice (PBody simple)), RNodes simple) => Recursively c (PLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c (FlatLine simple))) $
      withDict (recursively $ Proxy @(c (If simple))) $
        withDict (recursively $ Proxy @(c (Choice (PBody simple)))) Dict

instance RNodes simple => RNodes (PBody simple)
instance (c (PBody simple), Recursively c (PLine simple), RNodes simple) => Recursively c (PBody simple)

instance RNodes simple => RNodes (If simple)
instance (c (If simple), Recursively c (PBody simple), Recursively c (Else simple), RNodes simple) => Recursively c (If simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c (PBody simple))) $
      withDict (recursively $ Proxy @(c (Else simple))) Dict

instance RNodes simple => RNodes (Else simple)
instance (c (Else simple), Recursively c (If simple), Recursively c (PBody simple), RNodes simple) => Recursively c (Else simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c (If simple))) $
      withDict (recursively $ Proxy @(c (PBody simple))) Dict

instance RNodes simple => RNodes (CLoc simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (CLoc simple), Recursively c (FlatLine simple), Recursively c Expr, Recursively c (Choice (Const Int)), RNodes simple) => Recursively c (CLoc simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c (FlatLine simple))) $
      withDict (recursively $ Proxy @(c Expr)) $
        withDict (recursively $ Proxy @(c (Choice (Const Int)))) Dict

instance RNodes simple => RNodes (CLine simple)
instance (c (CLine simple), Recursively c (CLoc simple), RNodes simple) => Recursively c (CLine simple)

instance RNodes body => RNodes (Choice body)
instance (c (Choice body), Recursively c (Option body), RNodes body) => Recursively c (Choice body)

instance RNodes body => RNodes (Option body) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (c (Option body), Recursively c OptionMod, Recursively c Str, Recursively c body) => Recursively c (Option body) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c body)) Dict

instance RNodes OptionMod
instance (c OptionMod, Recursively c Expr) => Recursively c OptionMod

instance RNodes StartupSimpleCommand
instance (c StartupSimpleCommand, Recursively c Expr, Recursively c Str, Recursively c AchData, Recursively c SimpleCommand) => Recursively c StartupSimpleCommand

instance RNodes AchData
instance (c AchData, Recursively c Str) => Recursively c AchData

instance RNodes SimpleCommand
instance (c SimpleCommand, Recursively c Expr, Recursively c (Target Var), Recursively c SetExpr, Recursively c (Target Label), Recursively c SubArgs, Recursively c Str) => Recursively c SimpleCommand

instance RNodes SubArgs
instance (c SubArgs, Recursively c (Target Label), Recursively c Expr) => Recursively c SubArgs

instance RNodes (Target a)
instance (c (Target a), Recursively c Expr) => Recursively c (Target a)

instance RNodes SetExpr
instance (c SetExpr, Recursively c Expr) => Recursively c SetExpr

pBodyWitness :: Dict (Recursively HFunctor (PBody StartupSimpleCommand))
pBodyWitness = Dict

cLineWitness :: Dict (Recursively HFunctor (CLine StartupSimpleCommand))
cLineWitness = Dict
