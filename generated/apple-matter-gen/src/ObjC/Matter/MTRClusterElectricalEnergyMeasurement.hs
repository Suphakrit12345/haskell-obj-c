{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Energy Measurement    This cluster provides a mechanism for querying data about the electrical energy imported or provided by the server.
--
-- Generated bindings for @MTRClusterElectricalEnergyMeasurement@.
module ObjC.Matter.MTRClusterElectricalEnergyMeasurement
  ( MTRClusterElectricalEnergyMeasurement
  , IsMTRClusterElectricalEnergyMeasurement(..)
  , readAttributeAccuracyWithParams
  , readAttributeCumulativeEnergyImportedWithParams
  , readAttributeCumulativeEnergyExportedWithParams
  , readAttributePeriodicEnergyImportedWithParams
  , readAttributePeriodicEnergyExportedWithParams
  , readAttributeCumulativeEnergyResetWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeAccuracyWithParamsSelector
  , readAttributeCumulativeEnergyImportedWithParamsSelector
  , readAttributeCumulativeEnergyExportedWithParamsSelector
  , readAttributePeriodicEnergyImportedWithParamsSelector
  , readAttributePeriodicEnergyExportedWithParamsSelector
  , readAttributeCumulativeEnergyResetWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAccuracyWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeAccuracyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCumulativeEnergyImportedWithParams:@
readAttributeCumulativeEnergyImportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyImportedWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyImportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCumulativeEnergyExportedWithParams:@
readAttributeCumulativeEnergyExportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyExportedWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyExportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeriodicEnergyImportedWithParams:@
readAttributePeriodicEnergyImportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributePeriodicEnergyImportedWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributePeriodicEnergyImportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeriodicEnergyExportedWithParams:@
readAttributePeriodicEnergyExportedWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributePeriodicEnergyExportedWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributePeriodicEnergyExportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCumulativeEnergyResetWithParams:@
readAttributeCumulativeEnergyResetWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeCumulativeEnergyResetWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyResetWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRReadParams params) => mtrClusterElectricalEnergyMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalEnergyMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement => mtrClusterElectricalEnergyMeasurement -> IO (Id MTRClusterElectricalEnergyMeasurement)
init_ mtrClusterElectricalEnergyMeasurement  =
    sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterElectricalEnergyMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalEnergyMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalEnergyMeasurement mtrClusterElectricalEnergyMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalEnergyMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalEnergyMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterElectricalEnergyMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParamsSelector :: Selector
readAttributeAccuracyWithParamsSelector = mkSelector "readAttributeAccuracyWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithParams:@
readAttributeCumulativeEnergyImportedWithParamsSelector :: Selector
readAttributeCumulativeEnergyImportedWithParamsSelector = mkSelector "readAttributeCumulativeEnergyImportedWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithParams:@
readAttributeCumulativeEnergyExportedWithParamsSelector :: Selector
readAttributeCumulativeEnergyExportedWithParamsSelector = mkSelector "readAttributeCumulativeEnergyExportedWithParams:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithParams:@
readAttributePeriodicEnergyImportedWithParamsSelector :: Selector
readAttributePeriodicEnergyImportedWithParamsSelector = mkSelector "readAttributePeriodicEnergyImportedWithParams:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithParams:@
readAttributePeriodicEnergyExportedWithParamsSelector :: Selector
readAttributePeriodicEnergyExportedWithParamsSelector = mkSelector "readAttributePeriodicEnergyExportedWithParams:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithParams:@
readAttributeCumulativeEnergyResetWithParamsSelector :: Selector
readAttributeCumulativeEnergyResetWithParamsSelector = mkSelector "readAttributeCumulativeEnergyResetWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

