{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST (module AST.Expr, module AST) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics
import Hyper
import Hyper.Class.Recursive

import AST.Expr
import AST.TH

data Line cmd h
  = Text (Str h)
  | EmptyLine
  | Command (cmd h)
  deriving (Generic)

type PLine cmd = Line (cmd (PBody cmd))

newtype PBody cmd h = PBody [h :# PLine cmd]
  deriving (Generic)

data CLine (cmd :: (HyperType -> HyperType)) (h :: AHyperType)
  = CLoc (h :# Line (cmd (Const Int)))
  | Jump Int
  deriving (Generic)

data Command body h
  = HideReuse
  | Temp Var (h :# Expr)
  | Set (Target Var h) (SetExpr h)
  | Delete Var
  | InputNumber (Target Var h) Int Int
  | InputText (Target Var h)
  | Print (Target Var h)
  | Rand (Target Var h) Int Int
  | Goto (Target Label h)
  | GotoScene SceneName (Maybe (Target Label h))
  | Gosub (SubArgs h)
  | GosubScene SceneName (Maybe (SubArgs h))
  | Params (NonEmpty (Target Var h))
  | Return
  | GotoRandomScene (NonEmpty SceneName)
  | Finish (Maybe (Str h))
  | LineBreak
  | PageBreak (Maybe (Str h))
  | StatChart (NonEmpty Stat)
  | Achieve Achievement
  | CheckAchievements
  | Ending
  | IfCmd (If body h)
  | Choice ChoiceMode (NonEmpty (h :# Option body))
  deriving (Generic)

data StartupCommand body h
  = NormalCommand (Command body h)
  | Create Var (h :# Expr)
  | SceneLst (NonEmpty SceneName)
  | Title (Str h)
  | Author (Str h)
  | Achievement Achievement (AchData h)
  deriving (Generic)

data If body h = If (h :# Expr) (body h) (Maybe (h :# Else body))
  deriving (Generic)

data Else body h
  = Elseif (If body h)
  | Else (body h)
  deriving (Generic)

data ChoiceMode = ChoiceMode | FakeChoiceMode
  deriving (Generic, Eq, Ord, Show)

data Option body h = Option
  { _optionId :: Int
  , _reuseMods :: [h :# ReuseMod]
  , _ifMods :: [h :# IfMod]
  , _optionText :: Str h
  , _optionBody :: body h
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

data AchData h = AchData
  { _visible :: Bool
  , _points :: Int
  , _title :: Str h
  , _preDesc :: Str h
  , _postDesc :: Str h
  }
  deriving (Generic)

data SetExpr h
  = NormalSet (h :# Expr)
  | ModSet BinOp (h :# Expr)
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

newtype Label = L {getL :: ByteString}
  deriving (Show, Eq, Generic)

newtype SceneName = SN {getSN :: ByteString}
  deriving (Show, Generic)

newtype Achievement = A {getA :: ByteString}
  deriving (Show, Generic)

makeAll [''Command, ''StartupCommand, ''If, ''Else, ''Line, ''PBody, ''CLine, ''Option, ''ReuseMod, ''IfMod, ''AchData, ''SetExpr, ''SubArgs]

instance (RNodes cmd) => RNodes (Line cmd) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @cmd) Dict
instance (Recursively c cmd, c (Line cmd), c Expr, c Multirep) => Recursively c (Line cmd) where
  recursively _ = withDict (recursively $ Proxy @(c cmd)) Dict
instance (RTraversable cmd) => RTraversable (Line cmd) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @cmd) Dict

instance (RNodes (cmd (Const Int))) => RNodes (CLine cmd)
instance (Recursively c (cmd (Const Int)), c (CLine cmd), c (Line (cmd (Const Int))), c Expr, c Multirep) => Recursively c (CLine cmd)
instance (RTraversable (cmd (Const Int))) => RTraversable (CLine cmd)

instance (RNodes (cmd (PBody cmd))) => RNodes (PBody cmd)
instance (Recursively c (cmd (PBody cmd)), c (Line (cmd (PBody cmd))), c (PBody cmd), c Expr, c Multirep) => Recursively c (PBody cmd)
instance (RTraversable (cmd (PBody cmd))) => RTraversable (PBody cmd)

instance (RNodes body) => RNodes (Command body) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Command body), c Expr, c Multirep, c (Else body), c (Option body), c IfMod, c ReuseMod) => Recursively c (Command body) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Command body) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (StartupCommand body) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (StartupCommand body), c Expr, c Multirep, c (Else body), c (Option body), c IfMod, c ReuseMod) => Recursively c (StartupCommand body) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (StartupCommand body) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (Else body) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Else body), c Expr, c Multirep) => Recursively c (Else body) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Else body) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (Option body) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Option body), c Expr, c Multirep, c IfMod, c ReuseMod) => Recursively c (Option body) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Option body) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance RNodes ReuseMod
instance (c ReuseMod) => Recursively c ReuseMod
instance RTraversable ReuseMod

instance RNodes IfMod
instance (c IfMod, c Expr, c Multirep) => Recursively c IfMod
instance RTraversable IfMod
