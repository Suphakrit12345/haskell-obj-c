{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSystemModeChangeEvent@.
module ObjC.Matter.MTRThermostatClusterSystemModeChangeEvent
  ( MTRThermostatClusterSystemModeChangeEvent
  , IsMTRThermostatClusterSystemModeChangeEvent(..)
  , previousSystemMode
  , setPreviousSystemMode
  , currentSystemMode
  , setCurrentSystemMode
  , previousSystemModeSelector
  , setPreviousSystemModeSelector
  , currentSystemModeSelector
  , setCurrentSystemModeSelector


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

-- | @- previousSystemMode@
previousSystemMode :: IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent => mtrThermostatClusterSystemModeChangeEvent -> IO (Id NSNumber)
previousSystemMode mtrThermostatClusterSystemModeChangeEvent  =
    sendMsg mtrThermostatClusterSystemModeChangeEvent (mkSelector "previousSystemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousSystemMode:@
setPreviousSystemMode :: (IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent, IsNSNumber value) => mtrThermostatClusterSystemModeChangeEvent -> value -> IO ()
setPreviousSystemMode mtrThermostatClusterSystemModeChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSystemModeChangeEvent (mkSelector "setPreviousSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentSystemMode@
currentSystemMode :: IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent => mtrThermostatClusterSystemModeChangeEvent -> IO (Id NSNumber)
currentSystemMode mtrThermostatClusterSystemModeChangeEvent  =
    sendMsg mtrThermostatClusterSystemModeChangeEvent (mkSelector "currentSystemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentSystemMode:@
setCurrentSystemMode :: (IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent, IsNSNumber value) => mtrThermostatClusterSystemModeChangeEvent -> value -> IO ()
setCurrentSystemMode mtrThermostatClusterSystemModeChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSystemModeChangeEvent (mkSelector "setCurrentSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousSystemMode@
previousSystemModeSelector :: Selector
previousSystemModeSelector = mkSelector "previousSystemMode"

-- | @Selector@ for @setPreviousSystemMode:@
setPreviousSystemModeSelector :: Selector
setPreviousSystemModeSelector = mkSelector "setPreviousSystemMode:"

-- | @Selector@ for @currentSystemMode@
currentSystemModeSelector :: Selector
currentSystemModeSelector = mkSelector "currentSystemMode"

-- | @Selector@ for @setCurrentSystemMode:@
setCurrentSystemModeSelector :: Selector
setCurrentSystemModeSelector = mkSelector "setCurrentSystemMode:"

