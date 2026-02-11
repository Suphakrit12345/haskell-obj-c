{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The object you use to observe changes to the current configuration.
--
-- Use this class to start and stop observing the current configuration. For example, you can opt to disable private browsing in your web browser’s view controller when ``STScreenTimeConfiguration/enforcesChildRestrictions`` is @true@.
--
-- Generated bindings for @STScreenTimeConfigurationObserver@.
module ObjC.ScreenTime.STScreenTimeConfigurationObserver
  ( STScreenTimeConfigurationObserver
  , IsSTScreenTimeConfigurationObserver(..)
  , initWithUpdateQueue
  , startObserving
  , stopObserving
  , init_
  , new
  , configuration
  , initWithUpdateQueueSelector
  , startObservingSelector
  , stopObservingSelector
  , initSelector
  , newSelector
  , configurationSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a configuration observer that reports updates on the queue you specify.
--
-- - Parameters:   - updateQueue: The queue on which to report updates.
--
-- ObjC selector: @- initWithUpdateQueue:@
initWithUpdateQueue :: (IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver, IsNSObject updateQueue) => stScreenTimeConfigurationObserver -> updateQueue -> IO (Id STScreenTimeConfigurationObserver)
initWithUpdateQueue stScreenTimeConfigurationObserver  updateQueue =
  withObjCPtr updateQueue $ \raw_updateQueue ->
      sendMsg stScreenTimeConfigurationObserver (mkSelector "initWithUpdateQueue:") (retPtr retVoid) [argPtr (castPtr raw_updateQueue :: Ptr ())] >>= ownedObject . castPtr

-- | Starts observing changes to the current configuration.
--
-- ObjC selector: @- startObserving@
startObserving :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO ()
startObserving stScreenTimeConfigurationObserver  =
    sendMsg stScreenTimeConfigurationObserver (mkSelector "startObserving") retVoid []

-- | Stops observing changes to the current configuration.
--
-- ObjC selector: @- stopObserving@
stopObserving :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO ()
stopObserving stScreenTimeConfigurationObserver  =
    sendMsg stScreenTimeConfigurationObserver (mkSelector "stopObserving") retVoid []

-- | @- init@
init_ :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO (Id STScreenTimeConfigurationObserver)
init_ stScreenTimeConfigurationObserver  =
    sendMsg stScreenTimeConfigurationObserver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id STScreenTimeConfigurationObserver)
new  =
  do
    cls' <- getRequiredClass "STScreenTimeConfigurationObserver"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The configuration being observed.
--
-- ObjC selector: @- configuration@
configuration :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO (Id STScreenTimeConfiguration)
configuration stScreenTimeConfigurationObserver  =
    sendMsg stScreenTimeConfigurationObserver (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUpdateQueue:@
initWithUpdateQueueSelector :: Selector
initWithUpdateQueueSelector = mkSelector "initWithUpdateQueue:"

-- | @Selector@ for @startObserving@
startObservingSelector :: Selector
startObservingSelector = mkSelector "startObserving"

-- | @Selector@ for @stopObserving@
stopObservingSelector :: Selector
stopObservingSelector = mkSelector "stopObserving"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

