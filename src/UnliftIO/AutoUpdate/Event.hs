{-# LANGUAGE RecordWildCards #-}

module UnliftIO.AutoUpdate.Event
  ( -- * Creation
    mkAutoUpdate
  , mkAutoUpdateWithModify

    -- * Internal
  , UpdateState (..)
  , mkClosableAutoUpdate
  , mkClosableAutoUpdate'
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)
import UnliftIO (MonadUnliftIO)
import UnliftIO.AutoUpdate.Types
import UnliftIO.IORef
import UnliftIO.STM

--------------------------------------------------------------------------------

{- | Generate an action which will either read from an automatically
 updated value, or run the update action in the current thread.

 @since 0.1.0
-}
mkAutoUpdate :: MonadUnliftIO m => UpdateSettings m a -> m (m a)
mkAutoUpdate = mkAutoUpdateThings $ \g _ _ -> g

{- | Generate an action which will either read from an automatically
 updated value, or run the update action in the current thread if
 the first time or the provided modify action after that.

 @since 0.1.0
-}
mkAutoUpdateWithModify :: MonadUnliftIO m => UpdateSettings m a -> (a -> m a) -> m (m a)
mkAutoUpdateWithModify = mkAutoUpdateThingsWithModify (\g _ _ -> g)

--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data UpdateState m a =
    UpdateState
    { usUpdateAction_   :: a -> m a
    , usLastResult_     :: IORef a
    , usIntervalMicro_  :: Int
    , usTimeHasCome_    :: TVar Bool
    , usDeleteTimeout_  :: IORef (m ())
    }
{- FOURMOLU_ENABLE -}

--------------------------------------------------------------------------------

mkAutoUpdateThings ::
  MonadUnliftIO m =>
  (m a -> m () -> UpdateState m a -> b) ->
  UpdateSettings m a ->
  m b
mkAutoUpdateThings mk settings@UpdateSettings {..} =
  mkAutoUpdateThingsWithModify mk settings (const updateAction)

mkAutoUpdateThingsWithModify ::
  MonadUnliftIO m =>
  (m a -> m () -> UpdateState m a -> b) ->
  UpdateSettings m a ->
  (a -> m a) ->
  m b
mkAutoUpdateThingsWithModify mk settings update1 = do
  us <- openUpdateState settings update1
  pure $ mk (getUpdateResult us) (closeUpdateState us) us

--------------------------------------------------------------------------------

{- $setup
 >>> :seti -XNumericUnderscores
 >>> import Control.Concurrent
-}

{- |
 >>> iref <- newIORef (0 :: Int)
 >>> action = modifyIORef iref (+ 1) >> readIORef iref
 >>> (getValue, closeState) <- mkClosableAutoUpdate $ defaultUpdateSettings { updateFreq = 200_000, updateAction = action }
 >>> getValue
 1
 >>> threadDelay 100_000 >> getValue
 1
 >>> threadDelay 200_000 >> getValue
 2
 >>> closeState
-}
mkClosableAutoUpdate :: MonadUnliftIO m => UpdateSettings m a -> m (m a, m ())
mkClosableAutoUpdate = mkAutoUpdateThings $ \g c _ -> (g, c)

-- | provide `UpdateState` for debugging
mkClosableAutoUpdate' :: MonadUnliftIO m => UpdateSettings m a -> m (m a, m (), UpdateState m a)
mkClosableAutoUpdate' = mkAutoUpdateThings (,,)

--------------------------------------------------------------------------------

mkDeleteTimeout :: MonadUnliftIO m => TVar Bool -> Int -> m (m ())
mkDeleteTimeout thc micro = do
  mgr <- liftIO getSystemTimerManager
  key <- liftIO $ registerTimeout mgr micro (atomically $ writeTVar thc True)
  pure $ liftIO $ unregisterTimeout mgr key

openUpdateState :: MonadUnliftIO m => UpdateSettings m a -> (a -> m a) -> m (UpdateState m a)
openUpdateState UpdateSettings {..} update1 = do
  thc <- newTVarIO False
  UpdateState update1
    <$> (newIORef =<< updateAction)
    <*> pure updateFreq
    <*> pure thc
    <*> (newIORef =<< mkDeleteTimeout thc updateFreq)

closeUpdateState :: MonadUnliftIO m => UpdateState m a -> m ()
closeUpdateState UpdateState {..} = do
  join $ readIORef usDeleteTimeout_

onceOnTimeHasCome :: MonadUnliftIO m => UpdateState m a -> m () -> m ()
onceOnTimeHasCome UpdateState {..} action = do
  join . atomically $ do
    timeHasCome <- readTVar usTimeHasCome_
    when timeHasCome $ writeTVar usTimeHasCome_ False
    pure $ when timeHasCome action

getUpdateResult :: MonadUnliftIO m => UpdateState m a -> m a
getUpdateResult us@UpdateState {..} = do
  onceOnTimeHasCome us $ do
    writeIORef usLastResult_ =<< usUpdateAction_ =<< readIORef usLastResult_
    writeIORef usDeleteTimeout_ =<< mkDeleteTimeout usTimeHasCome_ usIntervalMicro_
  readIORef usLastResult_
