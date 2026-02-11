{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Power Measurement    This cluster provides a mechanism for querying data about electrical power as measured by the server.
--
-- Generated bindings for @MTRClusterElectricalPowerMeasurement@.
module ObjC.Matter.MTRClusterElectricalPowerMeasurement
  ( MTRClusterElectricalPowerMeasurement
  , IsMTRClusterElectricalPowerMeasurement(..)
  , readAttributePowerModeWithParams
  , readAttributeNumberOfMeasurementTypesWithParams
  , readAttributeAccuracyWithParams
  , readAttributeRangesWithParams
  , readAttributeVoltageWithParams
  , readAttributeActiveCurrentWithParams
  , readAttributeReactiveCurrentWithParams
  , readAttributeApparentCurrentWithParams
  , readAttributeActivePowerWithParams
  , readAttributeReactivePowerWithParams
  , readAttributeApparentPowerWithParams
  , readAttributeRMSVoltageWithParams
  , readAttributeRMSCurrentWithParams
  , readAttributeRMSPowerWithParams
  , readAttributeFrequencyWithParams
  , readAttributeHarmonicCurrentsWithParams
  , readAttributeHarmonicPhasesWithParams
  , readAttributePowerFactorWithParams
  , readAttributeNeutralCurrentWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributePowerModeWithParamsSelector
  , readAttributeNumberOfMeasurementTypesWithParamsSelector
  , readAttributeAccuracyWithParamsSelector
  , readAttributeRangesWithParamsSelector
  , readAttributeVoltageWithParamsSelector
  , readAttributeActiveCurrentWithParamsSelector
  , readAttributeReactiveCurrentWithParamsSelector
  , readAttributeApparentCurrentWithParamsSelector
  , readAttributeActivePowerWithParamsSelector
  , readAttributeReactivePowerWithParamsSelector
  , readAttributeApparentPowerWithParamsSelector
  , readAttributeRMSVoltageWithParamsSelector
  , readAttributeRMSCurrentWithParamsSelector
  , readAttributeRMSPowerWithParamsSelector
  , readAttributeFrequencyWithParamsSelector
  , readAttributeHarmonicCurrentsWithParamsSelector
  , readAttributeHarmonicPhasesWithParamsSelector
  , readAttributePowerFactorWithParamsSelector
  , readAttributeNeutralCurrentWithParamsSelector
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

-- | @- readAttributePowerModeWithParams:@
readAttributePowerModeWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerModeWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributePowerModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfMeasurementTypesWithParams:@
readAttributeNumberOfMeasurementTypesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeNumberOfMeasurementTypesWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeNumberOfMeasurementTypesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAccuracyWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeAccuracyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRangesWithParams:@
readAttributeRangesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRangesWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeRangesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVoltageWithParams:@
readAttributeVoltageWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeVoltageWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveCurrentWithParams:@
readAttributeActiveCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeActiveCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactiveCurrentWithParams:@
readAttributeReactiveCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeReactiveCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApparentCurrentWithParams:@
readAttributeApparentCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentCurrentWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeApparentCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeActivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeReactivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeApparentPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRMSVoltageWithParams:@
readAttributeRMSVoltageWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSVoltageWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRMSCurrentWithParams:@
readAttributeRMSCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSCurrentWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRMSPowerWithParams:@
readAttributeRMSPowerWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeRMSPowerWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFrequencyWithParams:@
readAttributeFrequencyWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeFrequencyWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHarmonicCurrentsWithParams:@
readAttributeHarmonicCurrentsWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicCurrentsWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeHarmonicCurrentsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHarmonicPhasesWithParams:@
readAttributeHarmonicPhasesWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicPhasesWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeHarmonicPhasesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributePowerFactorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeNeutralCurrentWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeNeutralCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRReadParams params) => mtrClusterElectricalPowerMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalPowerMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement => mtrClusterElectricalPowerMeasurement -> IO (Id MTRClusterElectricalPowerMeasurement)
init_ mtrClusterElectricalPowerMeasurement  =
    sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterElectricalPowerMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalPowerMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalPowerMeasurement mtrClusterElectricalPowerMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalPowerMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalPowerMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterElectricalPowerMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePowerModeWithParams:@
readAttributePowerModeWithParamsSelector :: Selector
readAttributePowerModeWithParamsSelector = mkSelector "readAttributePowerModeWithParams:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithParams:@
readAttributeNumberOfMeasurementTypesWithParamsSelector :: Selector
readAttributeNumberOfMeasurementTypesWithParamsSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithParams:"

-- | @Selector@ for @readAttributeAccuracyWithParams:@
readAttributeAccuracyWithParamsSelector :: Selector
readAttributeAccuracyWithParamsSelector = mkSelector "readAttributeAccuracyWithParams:"

-- | @Selector@ for @readAttributeRangesWithParams:@
readAttributeRangesWithParamsSelector :: Selector
readAttributeRangesWithParamsSelector = mkSelector "readAttributeRangesWithParams:"

-- | @Selector@ for @readAttributeVoltageWithParams:@
readAttributeVoltageWithParamsSelector :: Selector
readAttributeVoltageWithParamsSelector = mkSelector "readAttributeVoltageWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentWithParams:@
readAttributeActiveCurrentWithParamsSelector :: Selector
readAttributeActiveCurrentWithParamsSelector = mkSelector "readAttributeActiveCurrentWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentWithParams:@
readAttributeReactiveCurrentWithParamsSelector :: Selector
readAttributeReactiveCurrentWithParamsSelector = mkSelector "readAttributeReactiveCurrentWithParams:"

-- | @Selector@ for @readAttributeApparentCurrentWithParams:@
readAttributeApparentCurrentWithParamsSelector :: Selector
readAttributeApparentCurrentWithParamsSelector = mkSelector "readAttributeApparentCurrentWithParams:"

-- | @Selector@ for @readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParamsSelector :: Selector
readAttributeActivePowerWithParamsSelector = mkSelector "readAttributeActivePowerWithParams:"

-- | @Selector@ for @readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParamsSelector :: Selector
readAttributeReactivePowerWithParamsSelector = mkSelector "readAttributeReactivePowerWithParams:"

-- | @Selector@ for @readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParamsSelector :: Selector
readAttributeApparentPowerWithParamsSelector = mkSelector "readAttributeApparentPowerWithParams:"

-- | @Selector@ for @readAttributeRMSVoltageWithParams:@
readAttributeRMSVoltageWithParamsSelector :: Selector
readAttributeRMSVoltageWithParamsSelector = mkSelector "readAttributeRMSVoltageWithParams:"

-- | @Selector@ for @readAttributeRMSCurrentWithParams:@
readAttributeRMSCurrentWithParamsSelector :: Selector
readAttributeRMSCurrentWithParamsSelector = mkSelector "readAttributeRMSCurrentWithParams:"

-- | @Selector@ for @readAttributeRMSPowerWithParams:@
readAttributeRMSPowerWithParamsSelector :: Selector
readAttributeRMSPowerWithParamsSelector = mkSelector "readAttributeRMSPowerWithParams:"

-- | @Selector@ for @readAttributeFrequencyWithParams:@
readAttributeFrequencyWithParamsSelector :: Selector
readAttributeFrequencyWithParamsSelector = mkSelector "readAttributeFrequencyWithParams:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithParams:@
readAttributeHarmonicCurrentsWithParamsSelector :: Selector
readAttributeHarmonicCurrentsWithParamsSelector = mkSelector "readAttributeHarmonicCurrentsWithParams:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithParams:@
readAttributeHarmonicPhasesWithParamsSelector :: Selector
readAttributeHarmonicPhasesWithParamsSelector = mkSelector "readAttributeHarmonicPhasesWithParams:"

-- | @Selector@ for @readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParamsSelector :: Selector
readAttributePowerFactorWithParamsSelector = mkSelector "readAttributePowerFactorWithParams:"

-- | @Selector@ for @readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParamsSelector :: Selector
readAttributeNeutralCurrentWithParamsSelector = mkSelector "readAttributeNeutralCurrentWithParams:"

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

