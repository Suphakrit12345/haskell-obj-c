{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct
  ( MTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct
  , IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct(..)
  , importedResetTimestamp
  , setImportedResetTimestamp
  , exportedResetTimestamp
  , setExportedResetTimestamp
  , importedResetSystime
  , setImportedResetSystime
  , exportedResetSystime
  , setExportedResetSystime
  , importedResetTimestampSelector
  , setImportedResetTimestampSelector
  , exportedResetTimestampSelector
  , setExportedResetTimestampSelector
  , importedResetSystimeSelector
  , setImportedResetSystimeSelector
  , exportedResetSystimeSelector
  , setExportedResetSystimeSelector


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

-- | @- importedResetTimestamp@
importedResetTimestamp :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
importedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "importedResetTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImportedResetTimestamp:@
setImportedResetTimestamp :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setImportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "setImportedResetTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exportedResetTimestamp@
exportedResetTimestamp :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
exportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "exportedResetTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExportedResetTimestamp:@
setExportedResetTimestamp :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setExportedResetTimestamp mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "setExportedResetTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- importedResetSystime@
importedResetSystime :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
importedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "importedResetSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImportedResetSystime:@
setImportedResetSystime :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setImportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "setImportedResetSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exportedResetSystime@
exportedResetSystime :: IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> IO (Id NSNumber)
exportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "exportedResetSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExportedResetSystime:@
setExportedResetSystime :: (IsMTRElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct -> value -> IO ()
setExportedResetSystime mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterCumulativeEnergyResetStruct (mkSelector "setExportedResetSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @importedResetTimestamp@
importedResetTimestampSelector :: Selector
importedResetTimestampSelector = mkSelector "importedResetTimestamp"

-- | @Selector@ for @setImportedResetTimestamp:@
setImportedResetTimestampSelector :: Selector
setImportedResetTimestampSelector = mkSelector "setImportedResetTimestamp:"

-- | @Selector@ for @exportedResetTimestamp@
exportedResetTimestampSelector :: Selector
exportedResetTimestampSelector = mkSelector "exportedResetTimestamp"

-- | @Selector@ for @setExportedResetTimestamp:@
setExportedResetTimestampSelector :: Selector
setExportedResetTimestampSelector = mkSelector "setExportedResetTimestamp:"

-- | @Selector@ for @importedResetSystime@
importedResetSystimeSelector :: Selector
importedResetSystimeSelector = mkSelector "importedResetSystime"

-- | @Selector@ for @setImportedResetSystime:@
setImportedResetSystimeSelector :: Selector
setImportedResetSystimeSelector = mkSelector "setImportedResetSystime:"

-- | @Selector@ for @exportedResetSystime@
exportedResetSystimeSelector :: Selector
exportedResetSystimeSelector = mkSelector "exportedResetSystime"

-- | @Selector@ for @setExportedResetSystime:@
setExportedResetSystimeSelector :: Selector
setExportedResetSystimeSelector = mkSelector "setExportedResetSystime:"

