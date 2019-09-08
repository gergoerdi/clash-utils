{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module Cactus.Clash.FetchM where

import Prelude ()
import Clash.Prelude hiding (lift)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Default

data Failure
    = Underrun
    | Overrun
    deriving Show

data Buffer n dat = Buffer
    { bufferContents :: Vec n dat
    , bufferNext :: Index (1 + n)
    }
    deriving (Show, Generic, NFDataX)

instance (KnownNat n, Default dat) => Default (Buffer n dat) where
    def = Buffer (pure def) 0

remember :: (KnownNat n) => Buffer n dat -> dat -> Buffer n dat
remember Buffer{..} x = Buffer
    { bufferContents = replace bufferNext x bufferContents
    , bufferNext = bufferNext + 1
    }

newtype FetchM n dat m a = FetchM{ unFetchM :: ReaderT (Buffer n dat) (StateT (Index (1 + n)) (ExceptT Failure m)) a }
    deriving newtype (Functor, Applicative, Monad)

runFetchM :: (Monad m, KnownNat n) => Buffer n dat -> FetchM n dat m a -> m (Either Failure a)
runFetchM buf act = runExceptT $ evalStateT (runReaderT (unFetchM act) buf) 0

fetch :: (Monad m, KnownNat n) => FetchM n dat m dat
fetch = do
    Buffer{..} <- FetchM ask
    idx <- FetchM get
    when (idx == maxBound) overrun
    when (idx >= bufferNext) underrun
    FetchM $ put $ idx + 1
    return $ bufferContents !! idx
  where
    overrun = FetchM . lift . lift . throwE $ Overrun
    underrun = FetchM . lift . lift . throwE $ Underrun
