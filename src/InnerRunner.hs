{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module InnerRunner (module InnerRunner) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)

import Run

data InnerRunHandler m = InnerRunHandler
  { _button :: ByteString -> m ()
  , _textBox :: m ByteString
  , _numberBox :: Int -> Int -> m Int
  , _chooser :: NonEmpty OutOption -> m OutOption
  , _printer :: ByteString -> m ()
  , _statPrinter :: NonEmpty OutStat -> m ()
  , _ender :: m ()
  }

makeFieldsNoPrefix ''InnerRunHandler

data PrintItem
  = PrintString ByteString
  | PrintStats (NonEmpty OutStat)

newtype InnerRunT m a = InnerRunT {runInnerRunT :: InnerRunHandler m -> StateT [PrintItem] m a}
  deriving (Functor)

instance Monad m => Applicative (InnerRunT m) where
  pure = InnerRunT . const . pure

  (<*>) = ap

instance Monad m => Monad (InnerRunT m) where
  return = pure

  InnerRunT f >>= k = InnerRunT $ \h -> do
    x <- f h
    runInnerRunT (k x) h

withPrint :: Monad m => (InnerRunHandler m -> m a) -> InnerRunT m a
withPrint x = InnerRunT $ \h -> do
  pis <- get
  lift $ forM_ pis $ \case
    PrintString t -> (h ^. printer) t
    PrintStats stats -> (h ^. statPrinter) stats
  res <- lift $ x h
  put []
  pure res

instance Monad m => InnerRun (InnerRunT m) where
  output t = InnerRunT $ const $ modify (++ [PrintString t])
  choice opts = withPrint $ \h -> (h ^. chooser) opts
  inputNumber lo hi = withPrint $ \h -> (h ^. numberBox) lo hi
  inputText = withPrint $ \h -> h ^. textBox
  statChart stats = InnerRunT $ const $ modify (++ [PrintStats stats])
  ending = withPrint $ \h -> h ^. ender
  showButton t = withPrint $ \h -> (h ^. button) t
