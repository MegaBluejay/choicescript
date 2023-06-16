{-# LANGUAGE TemplateHaskellQuotes #-}

module AST.TH (module AST.TH) where

import Generics.Constraints
import Hyper
import Language.Haskell.TH

makeAll :: [Name] -> Q [Dec]
makeAll names = ((++) . concat <$> traverse makeHTraversableAndBases names) <*> makeDerivings [''Show] names
