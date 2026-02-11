{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ozone Concentration Measurement    Attributes for reporting ozone concentration measurements
--
-- Generated bindings for @MTRClusterOzoneConcentrationMeasurement@.
module ObjC.Matter.MTRClusterOzoneConcentrationMeasurement
  ( MTRClusterOzoneConcentrationMeasurement
  , IsMTRClusterOzoneConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeUncertaintyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeMeasurementUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeMeasurementMediumWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeLevelValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRReadParams params) => mtrClusterOzoneConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOzoneConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement => mtrClusterOzoneConcentrationMeasurement -> IO (Id MTRClusterOzoneConcentrationMeasurement)
init_ mtrClusterOzoneConcentrationMeasurement  =
    sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOzoneConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOzoneConcentrationMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOzoneConcentrationMeasurement mtrClusterOzoneConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOzoneConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterOzoneConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterOzoneConcentrationMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOzoneConcentrationMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

