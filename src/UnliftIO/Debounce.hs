{- | This is an almost direct copy of [Control.Debounce](https://hackage.haskell.org/package/auto-update/docs/Control-Debounce.html)
 from the /auto-update/ package. The salient difference is that this module allows us to debounce actions in arbitrary
 monads using 'MonadUnliftIO'.

 Debounce an action, ensuring it doesn't occur more than once for a given
 period of time.

 This is useful as an optimization, for example to ensure that logs are only
 flushed to disk at most once per second.

 Example usage:

 @
 > printString <- 'mkDebounce' 'DI.defaultDebounceSettings'
                  { 'DI.debounceAction' = putStrLn "Running action"
                  , 'DI.debounceFreq' = 5000000 -- 5 seconds
                  , 'DI.debounceEdge' = 'DI.trailingEdge' -- Trigger on the trailing edge
                  }
 > printString
 Running action
 > printString
 \<Wait five seconds>
 Running action
 @

 See the fast-logger package ("System.Log.FastLogger") for real-world usage.

 @since 0.1.0
-}
module UnliftIO.Debounce
  ( -- * Creation
    mkDebounce

    -- * Settings
  , DI.DebounceSettings
  , defaultDebounceSettings

    -- ** Accessors
  , DI.debounceFreq
  , DI.debounceAction
  , DI.debounceEdge
  , DI.debounceThreadName

    -- ** Edge types
  , DI.DebounceEdge
  , DI.leadingEdge
  , DI.leadingMuteEdge
  , DI.trailingEdge
  , DI.trailingDelayEdge
  )
where

import Data.Functor.Identity
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (newMVar, threadDelay)
import qualified UnliftIO.Debounce.Internal as DI

{- | Default value for creating a 'DI.DebounceSettings'.

 @since 0.1.0
-}
defaultDebounceSettings :: DI.DebounceSettings Identity
defaultDebounceSettings =
  DI.DebounceSettings
    { DI.debounceFreq = 1000000
    , DI.debounceAction = return ()
    , DI.debounceEdge = DI.leadingEdge
    , DI.debounceThreadName = "Debounce"
    }

{- | Generate an action which will trigger the debounced action to be performed.

 /N.B. The generated action will always immediately return, regardless of the 'DI.debounceFreq',/
 /as the debounced action (and the delay\/cooldown) is always performed in a separate thread./

 @since 0.1.0
-}
mkDebounce :: MonadUnliftIO m => DI.DebounceSettings m -> m (m ())
mkDebounce settings = do
  baton <- newMVar ()
  DI.mkDebounceInternal baton threadDelay settings
