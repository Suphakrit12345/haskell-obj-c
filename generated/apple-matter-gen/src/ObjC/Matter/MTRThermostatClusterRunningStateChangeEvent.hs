{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterRunningStateChangeEvent@.
module ObjC.Matter.MTRThermostatClusterRunningStateChangeEvent
  ( MTRThermostatClusterRunningStateChangeEvent
  , IsMTRThermostatClusterRunningStateChangeEvent(..)
  , previousRunningState
  , setPreviousRunningState
  , currentRunningState
  , setCurrentRunningState
  , previousRunningStateSelector
  , setPreviousRunningStateSelector
  , currentRunningStateSelector
  , setCurrentRunningStateSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousRunningState@
previousRunningState :: IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent => mtrThermostatClusterRunningStateChangeEvent -> IO (Id NSNumber)
previousRunningState mtrThermostatClusterRunningStateChangeEvent  =
    sendMsg mtrThermostatClusterRunningStateChangeEvent (mkSelector "previousRunningState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousRunningState:@
setPreviousRunningState :: (IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningStateChangeEvent -> value -> IO ()
setPreviousRunningState mtrThermostatClusterRunningStateChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterRunningStateChangeEvent (mkSelector "setPreviousRunningState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentRunningState@
currentRunningState :: IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent => mtrThermostatClusterRunningStateChangeEvent -> IO (Id NSNumber)
currentRunningState mtrThermostatClusterRunningStateChangeEvent  =
    sendMsg mtrThermostatClusterRunningStateChangeEvent (mkSelector "currentRunningState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentRunningState:@
setCurrentRunningState :: (IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningStateChangeEvent -> value -> IO ()
setCurrentRunningState mtrThermostatClusterRunningStateChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterRunningStateChangeEvent (mkSelector "setCurrentRunningState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousRunningState@
previousRunningStateSelector :: Selector
previousRunningStateSelector = mkSelector "previousRunningState"

-- | @Selector@ for @setPreviousRunningState:@
setPreviousRunningStateSelector :: Selector
setPreviousRunningStateSelector = mkSelector "setPreviousRunningState:"

-- | @Selector@ for @currentRunningState@
currentRunningStateSelector :: Selector
currentRunningStateSelector = mkSelector "currentRunningState"

-- | @Selector@ for @setCurrentRunningState:@
setCurrentRunningStateSelector :: Selector
setCurrentRunningStateSelector = mkSelector "setCurrentRunningState:"

