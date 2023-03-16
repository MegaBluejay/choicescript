{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler (
  module Compiler,
) where

import Control.Applicative.Lift
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.DList (DList)
import Data.DList qualified as DL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hyper

import AST
import Parser

type TempProgram simple = (DList (Caret, Label, Pos), DList (CLine simple # Ann Loc))
type Program simple = (HashMap Label Pos, Vector (CLine simple # Ann Loc))

type CompM simple = WriterT (TempProgram simple) (State Int)

type instance LocType (CLoc simple) = Caret
type instance LocType (FlatLine simple) = Caret
type instance LocType (If simple) = Caret
type instance LocType (Choice body) = Caret

finLines :: Int -> DList (CLine simple # Ann Loc) -> Vector (CLine simple # Ann Loc)
finLines n = V.fromListN n . DL.toList

collectLbls :: DList (Caret, Label, Pos) -> HashMap Label (Pos, NonEmpty Caret)
collectLbls = foldr addLbl HM.empty
 where
  addLbl (loc, lbl, n) = flip HM.alter lbl . (Just .) $ \case
    Just (_, locs) -> (n, loc <| locs)
    Nothing -> (n, NE.singleton loc)

validateLbls :: HashMap Label (Pos, NonEmpty Caret) -> Errors (DList (Label, NonEmpty Caret)) (HashMap Label Pos)
validateLbls = HM.traverseWithKey $ \lbl (n, locs) ->
  case locs of
    _ :| [] -> pure n
    _ -> failure $ DL.singleton (lbl, locs)

finLbls :: DList (Caret, Label, Pos) -> Either [(Label, NonEmpty Caret)] (HashMap Label Pos)
finLbls = first DL.toList . runErrors . validateLbls . collectLbls

finalize :: (TempProgram simple, Int) -> Either [(Label, NonEmpty Caret)] (Program simple)
finalize ((lbls, ls), n) = (,) <$> finLbls lbls <*> pure (finLines n ls)

runCompM :: CompM simple () -> Either [(Label, NonEmpty Caret)] (Program simple)
runCompM = finalize . flip runState 0 . execWriterT

compileScene :: [Ann Loc # PLine simple] -> Either [(Label, NonEmpty Caret)] (Program simple)
compileScene = runCompM . mapM_ compLine

compSingle :: CLine simple # Ann Loc -> CompM simple ()
compSingle l = tell (DL.empty, DL.singleton l) *> modify succ

getPos :: CompM simple Pos
getPos = gets P

compLine :: Ann Loc # PLine simple -> CompM simple ()
compLine (Ann (Loc loc) l) = case l of
  PFlat flat -> compFlat $ Ann (Loc loc) flat
  PIf xif -> compTopIf $ Ann (Loc loc) xif
  PChoice c -> compChoice $ Ann (Loc loc) c

compFlat :: Ann Loc # FlatLine simple -> CompM simple ()
compFlat (Ann (Loc loc) flat) = case flat of
  Label lbl -> getPos >>= \n -> tell (DL.singleton (loc, lbl, n), DL.empty)
  _ -> compSingle $ CLoc $ Ann (Loc loc) $ CFlat flat

compIf :: Pos -> Ann Loc # If simple -> CompM simple ()
compIf end (Ann (Loc loc) (If e b els)) = mdo
  compSingle $ CLoc $ Ann (Loc loc) $ JumpUnless e next
  compBody end b
  next <- getPos
  mapM_ (compElse end) els

compTopIf :: Ann Loc # If simple -> CompM simple ()
compTopIf xif = mdo
  compIf end xif
  end <- getPos
  pure ()

compBody :: Pos -> PBody simple # Ann Loc -> CompM simple ()
compBody end (PBody ls) = do
  mapM_ compLine ls
  compSingle $ ImplicitJump end

compElse :: Pos -> Ann Loc # Else simple -> CompM simple ()
compElse end (Ann (Loc loc) els) = case els of
  Elseif xif -> compIf end $ Ann (Loc loc) xif
  Else b -> compBody end b

compChoice :: Ann Loc # Choice (PBody simple) -> CompM simple ()
compChoice (Ann (Loc loc) (Choice cm opts)) = mdo
  compSingle $ CLoc $ Ann (Loc loc) $ CChoice $ Choice cm opts'
  opts' <- mapM (compOption end) opts
  end <- getPos
  pure ()

compOption :: Pos -> Ann Loc # Option (PBody simple) -> CompM simple (Ann Loc # Option (Const Pos))
compOption end (Ann (Loc loc) (Option xid rms ims t b)) = do
  pos <- getPos
  compBody end b
  pure $ Ann (Loc loc) (Option xid rms ims t (Const pos))
