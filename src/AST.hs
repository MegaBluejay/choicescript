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
import Control.Lens (Wrapped)

data Line cmd e (h :: AHyperType)
  = EmptyLine
  | Text (Str # e)
  | Command (cmd e h)
  deriving (Generic)

data PCommand sim e h
  = PSimple (sim e)
  | PIf (If sim e h)
  | PChoice (Choice (PBody sim e) e h)
  deriving (Generic)

data CCommand sim e h
  = CSimple (sim e)
  | CChoice (Choice (Const Int) e h)
  | JumpUnless (e # Expr) Int
  deriving (Generic)

data CLine sim e h
  = CLoc (h :# Line (CCommand sim) e)
  | Jump Int
  deriving (Generic)

type PLine sim = Line (PCommand sim)

newtype PBody sim e h = PBody [h :# PLine sim e]
  deriving (Generic)

data If sim e (h :: AHyperType) = If (e # Expr) (PBody sim e h) (Maybe (h :# Else sim e))
  deriving (Generic)

data Else sim e (h :: AHyperType)
  = Elseif (If sim e h)
  | Else (PBody sim e h)
  deriving (Generic)

data Choice body e h = Choice ChoiceMode (NonEmpty (Option body e h))
  deriving (Generic)

data Simple e
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
  deriving (Generic)

data SimpleStartup e
  = NormalSimple (Simple e)
  | Create Var (e # Expr)
  | SceneLst (NonEmpty SceneName)
  | Title (Str # e)
  | Author (Str # e)
  | Achievement Achievement (AchData e)
  deriving (Generic)

data ChoiceMode = ChoiceMode | FakeChoiceMode
  deriving (Generic, Eq, Ord, Show)

data Option body e (h :: AHyperType) = Option
  { _optionId :: Int
  , _reuseMods :: [ReuseMod]
  , _ifMods :: [IfMod e]
  , _optionText :: Str # e
  , _optionBody :: body h
  }
  deriving (Generic)

data ReuseMod
  = HideReuseMod
  | AllowReuseMod
  | DisableReuseMod
  deriving (Generic)

data IfMod e
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

instance Wrapped Label
instance Wrapped SceneName
instance Wrapped Achievement

makeAll [''Line, ''PCommand, ''CCommand, ''CLine, ''PBody, ''If, ''Else, ''Choice, ''Option]

instance RNodes (CLine sim e)
instance (c (CLine sim e), c (CCommand sim e), c (Line (CCommand sim) e)) => Recursively c (CLine sim e)
instance RTraversable (CLine sim e)

instance (RNodes (cmd e)) => RNodes (Line cmd e) where
  recursiveHNodes _ = withDict (recursiveHNodes $ Proxy @(cmd e)) Dict
instance (Recursively c (cmd e), c (Line cmd e)) => Recursively c (Line cmd e) where
  recursively _ = withDict (recursively $ Proxy @(c (cmd e))) Dict
instance (RTraversable (cmd e)) => RTraversable (Line cmd e) where
  recursiveHTraversable _ = withDict (recursiveHTraversable $ Proxy @(cmd e)) Dict

instance RNodes (CCommand sim e)
instance (c (CCommand sim e)) => Recursively c (CCommand sim e)
instance RTraversable (CCommand sim e)

instance RNodes (PCommand sim e)
instance (c (PCommand sim e), c (Else sim e), c (Line (PCommand sim) e)) => Recursively c (PCommand sim e)
instance RTraversable (PCommand sim e)

instance RNodes (Else sim e)
instance (c (Else sim e), c (PCommand sim e), c (Line (PCommand sim) e)) => Recursively c (Else sim e)
instance RTraversable (Else sim e)
