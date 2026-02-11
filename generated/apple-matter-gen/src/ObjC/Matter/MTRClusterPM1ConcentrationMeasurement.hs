{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster PM1 Concentration Measurement    Attributes for reporting PM1 concentration measurements
--
-- Generated bindings for @MTRClusterPM1ConcentrationMeasurement@.
module ObjC.Matter.MTRClusterPM1ConcentrationMeasurement
  ( MTRClusterPM1ConcentrationMeasurement
  , IsMTRClusterPM1ConcentrationMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributePeakMeasuredValueWithParams
  , readAttributePeakMeasuredValueWindowWithParams
  , readAttributeAverageMeasuredValueWithParams
  , readAttributeAverageMeasuredValueWindowWithParams
  , readAttributeUncertaintyWithParams
  , readAttributeMeasurementUnitWithParams
  , readAttributeMeasurementMediumWithParams
  , readAttributeLevelValueWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeMeasuredValueWithParamsSelector
  , readAttributeMinMeasuredValueWithParamsSelector
  , readAttributeMaxMeasuredValueWithParamsSelector
  , readAttributePeakMeasuredValueWithParamsSelector
  , readAttributePeakMeasuredValueWindowWithParamsSelector
  , readAttributeAverageMeasuredValueWithParamsSelector
  , readAttributeAverageMeasuredValueWindowWithParamsSelector
  , readAttributeUncertaintyWithParamsSelector
  , readAttributeMeasurementUnitWithParamsSelector
  , readAttributeMeasurementMediumWithParamsSelector
  , readAttributeLevelValueWithParamsSelector
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

-- | @- readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeUncertaintyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeMeasurementUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeMeasurementMediumWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeLevelValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRReadParams params) => mtrClusterPM1ConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPM1ConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement => mtrClusterPM1ConcentrationMeasurement -> IO (Id MTRClusterPM1ConcentrationMeasurement)
init_ mtrClusterPM1ConcentrationMeasurement  =
    sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPM1ConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPM1ConcentrationMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPM1ConcentrationMeasurement mtrClusterPM1ConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPM1ConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterPM1ConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterPM1ConcentrationMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPM1ConcentrationMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParamsSelector :: Selector
readAttributeMeasuredValueWithParamsSelector = mkSelector "readAttributeMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParamsSelector :: Selector
readAttributeMinMeasuredValueWithParamsSelector = mkSelector "readAttributeMinMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParamsSelector :: Selector
readAttributeMaxMeasuredValueWithParamsSelector = mkSelector "readAttributeMaxMeasuredValueWithParams:"

-- | @Selector@ for @readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParamsSelector :: Selector
readAttributePeakMeasuredValueWithParamsSelector = mkSelector "readAttributePeakMeasuredValueWithParams:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParamsSelector :: Selector
readAttributePeakMeasuredValueWindowWithParamsSelector = mkSelector "readAttributePeakMeasuredValueWindowWithParams:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParamsSelector :: Selector
readAttributeAverageMeasuredValueWithParamsSelector = mkSelector "readAttributeAverageMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParamsSelector :: Selector
readAttributeAverageMeasuredValueWindowWithParamsSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithParams:"

-- | @Selector@ for @readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParamsSelector :: Selector
readAttributeUncertaintyWithParamsSelector = mkSelector "readAttributeUncertaintyWithParams:"

-- | @Selector@ for @readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParamsSelector :: Selector
readAttributeMeasurementUnitWithParamsSelector = mkSelector "readAttributeMeasurementUnitWithParams:"

-- | @Selector@ for @readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParamsSelector :: Selector
readAttributeMeasurementMediumWithParamsSelector = mkSelector "readAttributeMeasurementMediumWithParams:"

-- | @Selector@ for @readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParamsSelector :: Selector
readAttributeLevelValueWithParamsSelector = mkSelector "readAttributeLevelValueWithParams:"

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

