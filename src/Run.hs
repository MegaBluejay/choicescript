{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Run (module Run) where

import Control.Monad.Reader
import Hyper
import Hyper.Recurse

import AST
import Eval

class Monad m => MonadOuterRun m where
  uncondJump :: Int -> m ()

class (MonadCast m, MonadBounded m) => MonadRun m

class RTraversable h => Runnable h where
  run :: MonadRun m => h # Runned -> m ()

data Runned (h :: AHyperType) = Runned

class RunnableSimple sim where
  runSimple :: MonadRun m => sim (Annotated Int) -> m ()

instance Runnable (CCommand sim (Annotated Int)) where
  run = undefined

instance Runnable (Line (CCommand sim) (Annotated Int)) where
  run = undefined

class TopRunnable a where
  topRun :: (MonadOuterRun m, MonadRun (ReaderT Int m)) => a -> m ()

instance Recursively Runnable h => TopRunnable (Annotated Int # h) where
  topRun =
    withDict (recursively $ Proxy @(Runnable h)) $
      void . withAnn (Proxy @Runnable ##>> \i x -> Runned <$ runReaderT (run x) i)

innerRun :: (MonadOuterRun m, MonadRun (ReaderT Int m), HTraversable h, HNodesConstraint h (Recursively Runnable)) => h # Annotated Int -> m (h # Runned)
innerRun = htraverse $ Proxy @(Recursively Runnable) #> (Runned <$) . topRun

instance TopRunnable (CLine sim (Annotated Int) # Annotated Int) where
  topRun =
    innerRun >=> \case
      CLoc Runned -> pure ()
      Jump pos -> uncondJump pos
