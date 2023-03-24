{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST (
  module AST,
  module AST.Expr,
) where

import Control.Lens.Wrapped
import Data.ByteString (ByteString)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Hyper
import Hyper.Class.Recursive

import AST.Expr
import AST.TH

data FlatLine simple h
  = Text (Str h)
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
  | JumpUnless (h :# Expr) Pos
  | CChoice (Choice (Const Pos) h)
  deriving (Generic)

data CLine simple h
  = CLoc (h :# CLoc simple)
  | ImplicitJumpChoice ChoiceMode Pos
  | ImplicitJumpIf Pos
  deriving (Generic)

data Choice body h
  = Choice ChoiceMode (NonEmpty (h :# Option body))
  deriving (Generic)

data ChoiceMode = ChoiceMode | FakeChoiceMode
  deriving (Generic, Eq, Ord, Show)

data Option body h = Option
  { optionId :: Int
  , reuseMods :: [h :# ReuseMod]
  , ifMods :: [h :# IfMod]
  , optionText :: Str h
  , optionBody :: body h
  }
  deriving (Generic)

data ReuseMod (h :: AHyperType)
  = HideReuseMod
  | AllowReuseMod
  | DisableReuseMod
  deriving (Generic)

data IfMod h
  = IfMod (h :# Expr)
  | SelectableIfMod (h :# Expr)
  deriving (Generic)

data StartupSimpleCommand h
  = Create Var (h :# Expr)
  | SceneList (NonEmpty SceneName)
  | Title (Str h)
  | Author (Str h)
  | Achievement Achievement (AchData h)
  | NormalSimple (SimpleCommand h)
  deriving (Generic)

data AchData h = AchData
  { visible :: Bool
  , points :: Int
  , title :: Str h
  , preDesc :: Str h
  , postDesc :: Str h
  }
  deriving (Generic)

data SimpleCommand h
  = HideReuse
  | Temp Var (h :# Expr)
  | Set (Target Var h) (h :# SetExpr)
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
  | Finish (Maybe (Str h))
  | LineBreak
  | PageBreak (Maybe (Str h))
  | Link ByteString (Maybe (Str h))
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

newtype Pos = P {getP :: Int}
  deriving (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Num)

instance Wrapped Label
instance (t ~ ByteString) => Rewrapped Label t

instance Wrapped SceneName
instance (t ~ ByteString) => Rewrapped SceneName t

instance Wrapped Achievement
instance (t ~ ByteString) => Rewrapped Achievement t

instance Wrapped Pos
instance (t ~ Int) => Rewrapped Pos t

makeAll
  [ ''FlatLine
  , ''PLine
  , ''PBody
  , ''If
  , ''Else
  , ''Choice
  , ''Option
  , ''IfMod
  , ''ReuseMod
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
instance (c (FlatLine simple), c Expr, c Multirep, Recursively c simple) => Recursively c (FlatLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict
instance RTraversable simple => RTraversable (FlatLine simple) where
  {-# INLINE recursiveHTraversable #-}
  recursiveHTraversable _ =
    withDict (recursiveHTraversable $ Proxy @simple) Dict

instance RNodes simple => RNodes (PLine simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (PLine simple), c (Else simple), c (PBody simple), c (Option (PBody simple)), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c simple) => Recursively c (PLine simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict
instance RTraversable simple => RTraversable (PLine simple) where
  {-# INLINE recursiveHTraversable #-}
  recursiveHTraversable _ =
    withDict (recursiveHTraversable $ Proxy @simple) Dict

instance RNodes simple => RNodes (PBody simple)
instance (c (PBody simple), c (PLine simple), c (Else simple), c (Option (PBody simple)), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c simple) => Recursively c (PBody simple)
instance RTraversable simple => RTraversable (PBody simple)

instance RNodes simple => RNodes (If simple)
instance (c (If simple), c (PLine simple), c (Else simple), c (PBody simple), c (Option (PBody simple)), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c simple) => Recursively c (If simple)
instance RTraversable simple => RTraversable (If simple)

instance RNodes simple => RNodes (Else simple)
instance (c (Else simple), c (PLine simple), c (PBody simple), c (Option (PBody simple)), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c simple) => Recursively c (Else simple)
instance RTraversable simple => RTraversable (Else simple)

instance RNodes simple => RNodes (CLoc simple) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @simple) Dict
instance (c (CLoc simple), c (Option (Const Pos)), c IfMod, c ReuseMod, c Expr, c (Const Pos), c Multirep, Recursively c simple) => Recursively c (CLoc simple) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c simple)) Dict
instance RTraversable simple => RTraversable (CLoc simple) where
  {-# INLINE recursiveHTraversable #-}
  recursiveHTraversable _ =
    withDict (recursiveHTraversable $ Proxy @simple) Dict

instance RNodes simple => RNodes (CLine simple)
instance (c (CLine simple), c (CLoc simple), c (Option (Const Pos)), c IfMod, c ReuseMod, c Expr, c Multirep, c (Const Pos), Recursively c simple) => Recursively c (CLine simple)
instance RTraversable simple => RTraversable (CLine simple)

instance RNodes body => RNodes (Choice body)
instance (c (Choice body), c (Option body), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c body) => Recursively c (Choice body)
instance RTraversable body => RTraversable (Choice body)

instance RNodes body => RNodes (Option body) where
  {-# INLINE recursiveHNodes #-}
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (c (Option body), c IfMod, c ReuseMod, c Expr, c Multirep, Recursively c body) => Recursively c (Option body) where
  {-# INLINE recursively #-}
  recursively _ =
    withDict (recursively $ Proxy @(c body)) Dict
instance RTraversable body => RTraversable (Option body) where
  {-# INLINE recursiveHTraversable #-}
  recursiveHTraversable _ =
    withDict (recursiveHTraversable $ Proxy @body) Dict

instance RNodes IfMod
instance (c IfMod, c Expr, c Multirep) => Recursively c IfMod
instance RTraversable IfMod

instance RNodes ReuseMod
instance (c ReuseMod) => Recursively c ReuseMod
instance RTraversable ReuseMod

instance RNodes StartupSimpleCommand
instance (c StartupSimpleCommand, c SetExpr, c Expr, c Multirep) => Recursively c StartupSimpleCommand
instance RTraversable StartupSimpleCommand

instance RNodes AchData
instance (c AchData, c Expr, c Multirep) => Recursively c AchData
instance RTraversable AchData

instance RNodes SimpleCommand
instance (c SimpleCommand, c SetExpr, c Expr, c Multirep) => Recursively c SimpleCommand
instance RTraversable SimpleCommand

instance RNodes SubArgs
instance (c SubArgs, c Expr, c Multirep) => Recursively c SubArgs
instance RTraversable SubArgs

instance RNodes SetExpr
instance (c SetExpr, c Expr, c Multirep) => Recursively c SetExpr
instance RTraversable SetExpr

pBodyWitness :: Dict (Recursively HFunctor (PBody StartupSimpleCommand))
pBodyWitness = Dict

cLineWitness :: Dict (Recursively HFunctor (CLine StartupSimpleCommand))
cLineWitness = Dict
