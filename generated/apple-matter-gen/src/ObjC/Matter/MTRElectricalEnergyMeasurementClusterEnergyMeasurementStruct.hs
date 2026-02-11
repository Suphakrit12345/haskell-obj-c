{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct
  ( MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct
  , IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct(..)
  , energy
  , setEnergy
  , startTimestamp
  , setStartTimestamp
  , endTimestamp
  , setEndTimestamp
  , startSystime
  , setStartSystime
  , endSystime
  , setEndSystime
  , apparentEnergy
  , setApparentEnergy
  , reactiveEnergy
  , setReactiveEnergy
  , energySelector
  , setEnergySelector
  , startTimestampSelector
  , setStartTimestampSelector
  , endTimestampSelector
  , setEndTimestampSelector
  , startSystimeSelector
  , setStartSystimeSelector
  , endSystimeSelector
  , setEndSystimeSelector
  , apparentEnergySelector
  , setApparentEnergySelector
  , reactiveEnergySelector
  , setReactiveEnergySelector


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

-- | @- energy@
energy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
energy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "energy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergy:@
setEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTimestamp@
startTimestamp :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
startTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "startTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTimestamp:@
setStartTimestamp :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setStartTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setStartTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endTimestamp@
endTimestamp :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
endTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "endTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndTimestamp:@
setEndTimestamp :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEndTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setEndTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startSystime@
startSystime :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
startSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "startSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartSystime:@
setStartSystime :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setStartSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setStartSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endSystime@
endSystime :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
endSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "endSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndSystime:@
setEndSystime :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEndSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setEndSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- apparentEnergy@
apparentEnergy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
apparentEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "apparentEnergy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApparentEnergy:@
setApparentEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setApparentEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setApparentEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reactiveEnergy@
reactiveEnergy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
reactiveEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "reactiveEnergy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReactiveEnergy:@
setReactiveEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setReactiveEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct (mkSelector "setReactiveEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @energy@
energySelector :: Selector
energySelector = mkSelector "energy"

-- | @Selector@ for @setEnergy:@
setEnergySelector :: Selector
setEnergySelector = mkSelector "setEnergy:"

-- | @Selector@ for @startTimestamp@
startTimestampSelector :: Selector
startTimestampSelector = mkSelector "startTimestamp"

-- | @Selector@ for @setStartTimestamp:@
setStartTimestampSelector :: Selector
setStartTimestampSelector = mkSelector "setStartTimestamp:"

-- | @Selector@ for @endTimestamp@
endTimestampSelector :: Selector
endTimestampSelector = mkSelector "endTimestamp"

-- | @Selector@ for @setEndTimestamp:@
setEndTimestampSelector :: Selector
setEndTimestampSelector = mkSelector "setEndTimestamp:"

-- | @Selector@ for @startSystime@
startSystimeSelector :: Selector
startSystimeSelector = mkSelector "startSystime"

-- | @Selector@ for @setStartSystime:@
setStartSystimeSelector :: Selector
setStartSystimeSelector = mkSelector "setStartSystime:"

-- | @Selector@ for @endSystime@
endSystimeSelector :: Selector
endSystimeSelector = mkSelector "endSystime"

-- | @Selector@ for @setEndSystime:@
setEndSystimeSelector :: Selector
setEndSystimeSelector = mkSelector "setEndSystime:"

-- | @Selector@ for @apparentEnergy@
apparentEnergySelector :: Selector
apparentEnergySelector = mkSelector "apparentEnergy"

-- | @Selector@ for @setApparentEnergy:@
setApparentEnergySelector :: Selector
setApparentEnergySelector = mkSelector "setApparentEnergy:"

-- | @Selector@ for @reactiveEnergy@
reactiveEnergySelector :: Selector
reactiveEnergySelector = mkSelector "reactiveEnergy"

-- | @Selector@ for @setReactiveEnergy:@
setReactiveEnergySelector :: Selector
setReactiveEnergySelector = mkSelector "setReactiveEnergy:"

