{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Carbon Monoxide Concentration Measurement    Attributes for reporting carbon monoxide concentration measurements
--
-- Generated bindings for @MTRClusterCarbonMonoxideConcentrationMeasurement@.
module ObjC.Matter.MTRClusterCarbonMonoxideConcentrationMeasurement
  ( MTRClusterCarbonMonoxideConcentrationMeasurement
  , IsMTRClusterCarbonMonoxideConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeUncertaintyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeMeasurementUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeMeasurementMediumWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeLevelValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRReadParams params) => mtrClusterCarbonMonoxideConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCarbonMonoxideConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement => mtrClusterCarbonMonoxideConcentrationMeasurement -> IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
init_ mtrClusterCarbonMonoxideConcentrationMeasurement  =
    sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCarbonMonoxideConcentrationMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCarbonMonoxideConcentrationMeasurement mtrClusterCarbonMonoxideConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCarbonMonoxideConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterCarbonMonoxideConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterCarbonMonoxideConcentrationMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCarbonMonoxideConcentrationMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

