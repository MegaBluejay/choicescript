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

data Line (cmd :: HyperType -> HyperType) (e :: HyperType) (h :: AHyperType)
  = Text (Str # e)
  | EmptyLine
  | Command (cmd e h)
  deriving (Generic)

type PLine (cmd :: HyperType -> HyperType -> HyperType) (e :: HyperType) = Line (cmd (PBody cmd e)) e

newtype PBody (cmd :: HyperType -> HyperType -> HyperType) (e :: HyperType) (h :: AHyperType) = PBody [h :# PLine cmd e]
  deriving (Generic)

data CLine (cmd :: (HyperType -> HyperType -> HyperType)) e (h :: AHyperType)
  = CLoc (h :# Line (cmd (Const Int)) e)
  | Jump Int
  deriving (Generic)

data Command (body :: HyperType) (e :: HyperType) (h :: AHyperType)
  = HideReuse
  | Temp Var (e # Expr)
  | Set (Target Var # e) (SetExpr e)
  | Delete Var
  | InputNumber (Target Var # e) Int Int
  | InputText (Target Var # e)
  | Print (Target Var # e)
  | Rand (Target Var # e) Int Int
  | Goto (Target Label # e)
  | GotoScene SceneName (Maybe (Target Label # e))
  | Gosub (SubArgs e)
  | GosubScene SceneName (Maybe (SubArgs e))
  | Params (NonEmpty (Target Var # e))
  | Return
  | GotoRandomScene (NonEmpty SceneName)
  | Finish (Maybe (Str # e))
  | LineBreak
  | PageBreak (Maybe (Str # e))
  | StatChart (NonEmpty Stat)
  | Achieve Achievement
  | CheckAchievements
  | Ending
  | IfCmd (If body e h)
  | Choice ChoiceMode (NonEmpty (h :# Option body e))
  deriving (Generic)

data StartupCommand body e (h :: AHyperType)
  = NormalCommand (Command body e h)
  | Create Var (e # Expr)
  | SceneLst (NonEmpty SceneName)
  | Title (Str # e)
  | Author (Str # e)
  | Achievement Achievement (AchData e)
  deriving (Generic)

data If body e (h :: AHyperType) = If (e # Expr) (body h) (Maybe (h :# Else body e))
  deriving (Generic)

data Else body e (h :: AHyperType)
  = Elseif (If body e h)
  | Else (body h)
  deriving (Generic)

data ChoiceMode = ChoiceMode | FakeChoiceMode
  deriving (Generic, Eq, Ord, Show)

data Option body e (h :: AHyperType) = Option
  { _optionId :: Int
  , _reuseMods :: [h :# ReuseMod]
  , _ifMods :: [h :# IfMod e]
  , _optionText :: Str # e
  , _optionBody :: body h
  }
  deriving (Generic)

data ReuseMod (h :: AHyperType)
  = HideReuseMod
  | AllowReuseMod
  | DisableReuseMod
  deriving (Generic)

data IfMod e (h :: AHyperType)
  = IfMod (e # Expr)
  | SelectableIfMod (e # Expr)
  deriving (Generic)

data AchData e = AchData
  { _visible :: Bool
  , _points :: Int
  , _title :: Str # e
  , _preDesc :: Str # e
  , _postDesc :: Str # e
  }
  deriving (Generic)

data SetExpr e
  = NormalSet (e # Expr)
  | ModSet BinOp (e # Expr)
  deriving (Generic)

data SubArgs e = SubArgs (Target Label # e) [e # Expr]
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

makeAll [''Command, ''StartupCommand, ''If, ''Else, ''Line, ''PBody, ''CLine, ''Option, ''ReuseMod, ''IfMod]

instance (RNodes (cmd e)) => RNodes (Line cmd e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @(cmd e)) Dict
instance (Recursively c (cmd e), c (Line cmd e)) => Recursively c (Line cmd e) where
  recursively _ = withDict (recursively $ Proxy @(c (cmd e))) Dict
instance (RTraversable (cmd e)) => RTraversable (Line cmd e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @(cmd e)) Dict

instance (RNodes (cmd (Const Int) e)) => RNodes (CLine cmd e)
instance (Recursively c (cmd (Const Int) e), c (CLine cmd e), c (Line (cmd (Const Int)) e)) => Recursively c (CLine cmd e)
instance (RTraversable (cmd (Const Int) e)) => RTraversable (CLine cmd e)

instance (RNodes (cmd (PBody cmd e) e)) => RNodes (PBody cmd e)
instance (Recursively c (cmd (PBody cmd e) e), c (Line (cmd (PBody cmd e)) e), c (PBody cmd e)) => Recursively c (PBody cmd e)
instance (RTraversable (cmd (PBody cmd e) e)) => RTraversable (PBody cmd e)

instance (RNodes body) => RNodes (Command body e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Command body e), c (Else body e), c (Option body e), c (IfMod e), c ReuseMod) => Recursively c (Command body e) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Command body e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (StartupCommand body e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (StartupCommand body e), c (Else body e), c (Option body e), c (IfMod e), c ReuseMod) => Recursively c (StartupCommand body e) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (StartupCommand body e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (Else body e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Else body e)) => Recursively c (Else body e) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Else body e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance (RNodes body) => RNodes (Option body e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @body) Dict
instance (Recursively c body, c (Option body e), c (IfMod e), c ReuseMod) => Recursively c (Option body e) where
  recursively _ = withDict (recursively $ Proxy @(c body)) Dict
instance (RTraversable body) => RTraversable (Option body e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @body) Dict

instance RNodes ReuseMod
instance (c ReuseMod) => Recursively c ReuseMod
instance RTraversable ReuseMod

instance RNodes (IfMod e)
instance (c (IfMod e)) => Recursively c (IfMod e)
instance RTraversable (IfMod e)
