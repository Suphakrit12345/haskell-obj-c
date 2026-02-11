{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterOccupancyChangeEvent@.
module ObjC.Matter.MTRThermostatClusterOccupancyChangeEvent
  ( MTRThermostatClusterOccupancyChangeEvent
  , IsMTRThermostatClusterOccupancyChangeEvent(..)
  , previousOccupancy
  , setPreviousOccupancy
  , currentOccupancy
  , setCurrentOccupancy
  , previousOccupancySelector
  , setPreviousOccupancySelector
  , currentOccupancySelector
  , setCurrentOccupancySelector


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

-- | @- previousOccupancy@
previousOccupancy :: IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent => mtrThermostatClusterOccupancyChangeEvent -> IO (Id NSNumber)
previousOccupancy mtrThermostatClusterOccupancyChangeEvent  =
    sendMsg mtrThermostatClusterOccupancyChangeEvent (mkSelector "previousOccupancy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousOccupancy:@
setPreviousOccupancy :: (IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent, IsNSNumber value) => mtrThermostatClusterOccupancyChangeEvent -> value -> IO ()
setPreviousOccupancy mtrThermostatClusterOccupancyChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterOccupancyChangeEvent (mkSelector "setPreviousOccupancy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentOccupancy@
currentOccupancy :: IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent => mtrThermostatClusterOccupancyChangeEvent -> IO (Id NSNumber)
currentOccupancy mtrThermostatClusterOccupancyChangeEvent  =
    sendMsg mtrThermostatClusterOccupancyChangeEvent (mkSelector "currentOccupancy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentOccupancy:@
setCurrentOccupancy :: (IsMTRThermostatClusterOccupancyChangeEvent mtrThermostatClusterOccupancyChangeEvent, IsNSNumber value) => mtrThermostatClusterOccupancyChangeEvent -> value -> IO ()
setCurrentOccupancy mtrThermostatClusterOccupancyChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterOccupancyChangeEvent (mkSelector "setCurrentOccupancy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousOccupancy@
previousOccupancySelector :: Selector
previousOccupancySelector = mkSelector "previousOccupancy"

-- | @Selector@ for @setPreviousOccupancy:@
setPreviousOccupancySelector :: Selector
setPreviousOccupancySelector = mkSelector "setPreviousOccupancy:"

-- | @Selector@ for @currentOccupancy@
currentOccupancySelector :: Selector
currentOccupancySelector = mkSelector "currentOccupancy"

-- | @Selector@ for @setCurrentOccupancy:@
setCurrentOccupancySelector :: Selector
setCurrentOccupancySelector = mkSelector "setCurrentOccupancy:"

