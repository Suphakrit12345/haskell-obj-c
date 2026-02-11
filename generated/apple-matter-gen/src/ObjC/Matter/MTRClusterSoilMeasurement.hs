{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Soil Measurement    This cluster provides an interface to soil measurement functionality, including configuration and provision of notifications of soil measurements.
--
-- Generated bindings for @MTRClusterSoilMeasurement@.
module ObjC.Matter.MTRClusterSoilMeasurement
  ( MTRClusterSoilMeasurement
  , IsMTRClusterSoilMeasurement(..)
  , readAttributeSoilMoistureMeasurementLimitsWithParams
  , readAttributeSoilMoistureMeasuredValueWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeSoilMoistureMeasurementLimitsWithParamsSelector
  , readAttributeSoilMoistureMeasuredValueWithParamsSelector
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

-- | @- readAttributeSoilMoistureMeasurementLimitsWithParams:@
readAttributeSoilMoistureMeasurementLimitsWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeSoilMoistureMeasurementLimitsWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeSoilMoistureMeasurementLimitsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoilMoistureMeasuredValueWithParams:@
readAttributeSoilMoistureMeasuredValueWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeSoilMoistureMeasuredValueWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeSoilMoistureMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRReadParams params) => mtrClusterSoilMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSoilMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoilMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement => mtrClusterSoilMeasurement -> IO (Id MTRClusterSoilMeasurement)
init_ mtrClusterSoilMeasurement  =
    sendMsg mtrClusterSoilMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterSoilMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSoilMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSoilMeasurement mtrClusterSoilMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSoilMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterSoilMeasurement)
initWithDevice_endpointID_queue mtrClusterSoilMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterSoilMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSoilMoistureMeasurementLimitsWithParams:@
readAttributeSoilMoistureMeasurementLimitsWithParamsSelector :: Selector
readAttributeSoilMoistureMeasurementLimitsWithParamsSelector = mkSelector "readAttributeSoilMoistureMeasurementLimitsWithParams:"

-- | @Selector@ for @readAttributeSoilMoistureMeasuredValueWithParams:@
readAttributeSoilMoistureMeasuredValueWithParamsSelector :: Selector
readAttributeSoilMoistureMeasuredValueWithParamsSelector = mkSelector "readAttributeSoilMoistureMeasuredValueWithParams:"

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

