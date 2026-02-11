{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterLocalTemperatureChangeEvent@.
module ObjC.Matter.MTRThermostatClusterLocalTemperatureChangeEvent
  ( MTRThermostatClusterLocalTemperatureChangeEvent
  , IsMTRThermostatClusterLocalTemperatureChangeEvent(..)
  , currentLocalTemperature
  , setCurrentLocalTemperature
  , currentLocalTemperatureSelector
  , setCurrentLocalTemperatureSelector


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

-- | @- currentLocalTemperature@
currentLocalTemperature :: IsMTRThermostatClusterLocalTemperatureChangeEvent mtrThermostatClusterLocalTemperatureChangeEvent => mtrThermostatClusterLocalTemperatureChangeEvent -> IO (Id NSNumber)
currentLocalTemperature mtrThermostatClusterLocalTemperatureChangeEvent  =
    sendMsg mtrThermostatClusterLocalTemperatureChangeEvent (mkSelector "currentLocalTemperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentLocalTemperature:@
setCurrentLocalTemperature :: (IsMTRThermostatClusterLocalTemperatureChangeEvent mtrThermostatClusterLocalTemperatureChangeEvent, IsNSNumber value) => mtrThermostatClusterLocalTemperatureChangeEvent -> value -> IO ()
setCurrentLocalTemperature mtrThermostatClusterLocalTemperatureChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterLocalTemperatureChangeEvent (mkSelector "setCurrentLocalTemperature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentLocalTemperature@
currentLocalTemperatureSelector :: Selector
currentLocalTemperatureSelector = mkSelector "currentLocalTemperature"

-- | @Selector@ for @setCurrentLocalTemperature:@
setCurrentLocalTemperatureSelector :: Selector
setCurrentLocalTemperatureSelector = mkSelector "setCurrentLocalTemperature:"

