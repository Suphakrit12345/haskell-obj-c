{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Water Heater Management    This cluster is used to allow clients to control the operation of a hot water heating appliance so that it can be used with energy management.
--
-- Generated bindings for @MTRClusterWaterHeaterManagement@.
module ObjC.Matter.MTRClusterWaterHeaterManagement
  ( MTRClusterWaterHeaterManagement
  , IsMTRClusterWaterHeaterManagement(..)
  , boostWithParams_expectedValues_expectedValueInterval_completion
  , cancelBoostWithParams_expectedValues_expectedValueInterval_completion
  , cancelBoostWithExpectedValues_expectedValueInterval_completion
  , readAttributeHeaterTypesWithParams
  , readAttributeHeatDemandWithParams
  , readAttributeTankVolumeWithParams
  , readAttributeEstimatedHeatRequiredWithParams
  , readAttributeTankPercentageWithParams
  , readAttributeBoostStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , boostWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelBoostWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeHeaterTypesWithParamsSelector
  , readAttributeHeatDemandWithParamsSelector
  , readAttributeTankVolumeWithParamsSelector
  , readAttributeEstimatedHeatRequiredWithParamsSelector
  , readAttributeTankPercentageWithParamsSelector
  , readAttributeBoostStateWithParamsSelector
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

-- | @- boostWithParams:expectedValues:expectedValueInterval:completion:@
boostWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterBoostParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
boostWithParams_expectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWaterHeaterManagement (mkSelector "boostWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelBoostWithParams:expectedValues:expectedValueInterval:completion:@
cancelBoostWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterCancelBoostParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelBoostWithParams_expectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWaterHeaterManagement (mkSelector "cancelBoostWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelBoostWithExpectedValues:expectedValueInterval:completion:@
cancelBoostWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelBoostWithExpectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWaterHeaterManagement (mkSelector "cancelBoostWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHeaterTypesWithParams:@
readAttributeHeaterTypesWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeHeaterTypesWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeHeaterTypesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHeatDemandWithParams:@
readAttributeHeatDemandWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeHeatDemandWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeHeatDemandWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTankVolumeWithParams:@
readAttributeTankVolumeWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeTankVolumeWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeTankVolumeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEstimatedHeatRequiredWithParams:@
readAttributeEstimatedHeatRequiredWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeEstimatedHeatRequiredWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeEstimatedHeatRequiredWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTankPercentageWithParams:@
readAttributeTankPercentageWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeTankPercentageWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeTankPercentageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBoostStateWithParams:@
readAttributeBoostStateWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeBoostStateWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeBoostStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWaterHeaterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWaterHeaterManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement => mtrClusterWaterHeaterManagement -> IO (Id MTRClusterWaterHeaterManagement)
init_ mtrClusterWaterHeaterManagement  =
    sendMsg mtrClusterWaterHeaterManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWaterHeaterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWaterHeaterManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWaterHeaterManagement -> device -> endpointID -> queue -> IO (Id MTRClusterWaterHeaterManagement)
initWithDevice_endpointID_queue mtrClusterWaterHeaterManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWaterHeaterManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostWithParams:expectedValues:expectedValueInterval:completion:@
boostWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
boostWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "boostWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelBoostWithParams:expectedValues:expectedValueInterval:completion:@
cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelBoostWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelBoostWithExpectedValues:expectedValueInterval:completion:@
cancelBoostWithExpectedValues_expectedValueInterval_completionSelector :: Selector
cancelBoostWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelBoostWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeHeaterTypesWithParams:@
readAttributeHeaterTypesWithParamsSelector :: Selector
readAttributeHeaterTypesWithParamsSelector = mkSelector "readAttributeHeaterTypesWithParams:"

-- | @Selector@ for @readAttributeHeatDemandWithParams:@
readAttributeHeatDemandWithParamsSelector :: Selector
readAttributeHeatDemandWithParamsSelector = mkSelector "readAttributeHeatDemandWithParams:"

-- | @Selector@ for @readAttributeTankVolumeWithParams:@
readAttributeTankVolumeWithParamsSelector :: Selector
readAttributeTankVolumeWithParamsSelector = mkSelector "readAttributeTankVolumeWithParams:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithParams:@
readAttributeEstimatedHeatRequiredWithParamsSelector :: Selector
readAttributeEstimatedHeatRequiredWithParamsSelector = mkSelector "readAttributeEstimatedHeatRequiredWithParams:"

-- | @Selector@ for @readAttributeTankPercentageWithParams:@
readAttributeTankPercentageWithParamsSelector :: Selector
readAttributeTankPercentageWithParamsSelector = mkSelector "readAttributeTankPercentageWithParams:"

-- | @Selector@ for @readAttributeBoostStateWithParams:@
readAttributeBoostStateWithParamsSelector :: Selector
readAttributeBoostStateWithParamsSelector = mkSelector "readAttributeBoostStateWithParams:"

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

