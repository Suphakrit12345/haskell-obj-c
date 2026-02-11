{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterRunningModeChangeEvent@.
module ObjC.Matter.MTRThermostatClusterRunningModeChangeEvent
  ( MTRThermostatClusterRunningModeChangeEvent
  , IsMTRThermostatClusterRunningModeChangeEvent(..)
  , previousRunningMode
  , setPreviousRunningMode
  , currentRunningMode
  , setCurrentRunningMode
  , previousRunningModeSelector
  , setPreviousRunningModeSelector
  , currentRunningModeSelector
  , setCurrentRunningModeSelector


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

-- | @- previousRunningMode@
previousRunningMode :: IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent => mtrThermostatClusterRunningModeChangeEvent -> IO (Id NSNumber)
previousRunningMode mtrThermostatClusterRunningModeChangeEvent  =
    sendMsg mtrThermostatClusterRunningModeChangeEvent (mkSelector "previousRunningMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousRunningMode:@
setPreviousRunningMode :: (IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningModeChangeEvent -> value -> IO ()
setPreviousRunningMode mtrThermostatClusterRunningModeChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterRunningModeChangeEvent (mkSelector "setPreviousRunningMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentRunningMode@
currentRunningMode :: IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent => mtrThermostatClusterRunningModeChangeEvent -> IO (Id NSNumber)
currentRunningMode mtrThermostatClusterRunningModeChangeEvent  =
    sendMsg mtrThermostatClusterRunningModeChangeEvent (mkSelector "currentRunningMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentRunningMode:@
setCurrentRunningMode :: (IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningModeChangeEvent -> value -> IO ()
setCurrentRunningMode mtrThermostatClusterRunningModeChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterRunningModeChangeEvent (mkSelector "setCurrentRunningMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousRunningMode@
previousRunningModeSelector :: Selector
previousRunningModeSelector = mkSelector "previousRunningMode"

-- | @Selector@ for @setPreviousRunningMode:@
setPreviousRunningModeSelector :: Selector
setPreviousRunningModeSelector = mkSelector "setPreviousRunningMode:"

-- | @Selector@ for @currentRunningMode@
currentRunningModeSelector :: Selector
currentRunningModeSelector = mkSelector "currentRunningMode"

-- | @Selector@ for @setCurrentRunningMode:@
setCurrentRunningModeSelector :: Selector
setCurrentRunningModeSelector = mkSelector "setCurrentRunningMode:"

