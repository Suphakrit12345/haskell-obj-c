{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Total Volatile Organic Compounds Concentration Measurement    Attributes for reporting total volatile organic compounds concentration measurements
--
-- Generated bindings for @MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement@.
module ObjC.Matter.MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement
  ( MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement
  , IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWithParams:@
readAttributePeakMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePeakMeasuredValueWindowWithParams:@
readAttributePeakMeasuredValueWindowWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributePeakMeasuredValueWindowWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWithParams:@
readAttributeAverageMeasuredValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageMeasuredValueWindowWithParams:@
readAttributeAverageMeasuredValueWindowWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageMeasuredValueWindowWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUncertaintyWithParams:@
readAttributeUncertaintyWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeUncertaintyWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeUncertaintyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementUnitWithParams:@
readAttributeMeasurementUnitWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementUnitWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeMeasurementUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasurementMediumWithParams:@
readAttributeMeasurementMediumWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementMediumWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeMeasurementMediumWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLevelValueWithParams:@
readAttributeLevelValueWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeLevelValueWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeLevelValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRReadParams params) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
init_ mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  =
    sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterTotalVolatileOrganicCompoundsConcentrationMeasurement)
initWithDevice_endpointID_queue mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTotalVolatileOrganicCompoundsConcentrationMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

