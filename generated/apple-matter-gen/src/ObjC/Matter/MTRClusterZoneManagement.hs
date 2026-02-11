{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Zone Management    This cluster provides an interface to manage regions of interest, or Zones, which can be either manufacturer or user defined.
--
-- Generated bindings for @MTRClusterZoneManagement@.
module ObjC.Matter.MTRClusterZoneManagement
  ( MTRClusterZoneManagement
  , IsMTRClusterZoneManagement(..)
  , createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion
  , updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion
  , removeZoneWithParams_expectedValues_expectedValueInterval_completion
  , createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion
  , removeTriggerWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxUserDefinedZonesWithParams
  , readAttributeMaxZonesWithParams
  , readAttributeZonesWithParams
  , readAttributeTriggersWithParams
  , readAttributeSensitivityMaxWithParams
  , readAttributeSensitivityWithParams
  , writeAttributeSensitivityWithValue_expectedValueInterval
  , writeAttributeSensitivityWithValue_expectedValueInterval_params
  , readAttributeTwoDCartesianMaxWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaxUserDefinedZonesWithParamsSelector
  , readAttributeMaxZonesWithParamsSelector
  , readAttributeZonesWithParamsSelector
  , readAttributeTriggersWithParamsSelector
  , readAttributeSensitivityMaxWithParamsSelector
  , readAttributeSensitivityWithParamsSelector
  , writeAttributeSensitivityWithValue_expectedValueIntervalSelector
  , writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector
  , readAttributeTwoDCartesianMaxWithParamsSelector
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

-- | @- createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterCreateTwoDCartesianZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterZoneManagement (mkSelector "createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterZoneManagement (mkSelector "updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeZoneWithParams:expectedValues:expectedValueInterval:completion:@
removeZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterRemoveZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterZoneManagement (mkSelector "removeZoneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:@
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterCreateOrUpdateTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterZoneManagement (mkSelector "createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeTriggerWithParams:expectedValues:expectedValueInterval:completion:@
removeTriggerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterRemoveTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeTriggerWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterZoneManagement (mkSelector "removeTriggerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxUserDefinedZonesWithParams:@
readAttributeMaxUserDefinedZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeMaxUserDefinedZonesWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeMaxUserDefinedZonesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxZonesWithParams:@
readAttributeMaxZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeMaxZonesWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeMaxZonesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeZonesWithParams:@
readAttributeZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeZonesWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeZonesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTriggersWithParams:@
readAttributeTriggersWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeTriggersWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeTriggersWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSensitivityMaxWithParams:@
readAttributeSensitivityMaxWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeSensitivityMaxWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeSensitivityMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSensitivityWithParams:@
readAttributeSensitivityWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeSensitivityWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeSensitivityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSensitivityWithValue:expectedValueInterval:@
writeAttributeSensitivityWithValue_expectedValueInterval :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSensitivityWithValue_expectedValueInterval mtrClusterZoneManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterZoneManagement (mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeSensitivityWithValue_expectedValueInterval_params :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterZoneManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSensitivityWithValue_expectedValueInterval_params mtrClusterZoneManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterZoneManagement (mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeTwoDCartesianMaxWithParams:@
readAttributeTwoDCartesianMaxWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeTwoDCartesianMaxWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeTwoDCartesianMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterZoneManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterZoneManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterZoneManagement mtrClusterZoneManagement => mtrClusterZoneManagement -> IO (Id MTRClusterZoneManagement)
init_ mtrClusterZoneManagement  =
    sendMsg mtrClusterZoneManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterZoneManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterZoneManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterZoneManagement -> device -> endpointID -> queue -> IO (Id MTRClusterZoneManagement)
initWithDevice_endpointID_queue mtrClusterZoneManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterZoneManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeZoneWithParams:expectedValues:expectedValueInterval:completion:@
removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:@
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeTriggerWithParams:expectedValues:expectedValueInterval:completion:@
removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeTriggerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithParams:@
readAttributeMaxUserDefinedZonesWithParamsSelector :: Selector
readAttributeMaxUserDefinedZonesWithParamsSelector = mkSelector "readAttributeMaxUserDefinedZonesWithParams:"

-- | @Selector@ for @readAttributeMaxZonesWithParams:@
readAttributeMaxZonesWithParamsSelector :: Selector
readAttributeMaxZonesWithParamsSelector = mkSelector "readAttributeMaxZonesWithParams:"

-- | @Selector@ for @readAttributeZonesWithParams:@
readAttributeZonesWithParamsSelector :: Selector
readAttributeZonesWithParamsSelector = mkSelector "readAttributeZonesWithParams:"

-- | @Selector@ for @readAttributeTriggersWithParams:@
readAttributeTriggersWithParamsSelector :: Selector
readAttributeTriggersWithParamsSelector = mkSelector "readAttributeTriggersWithParams:"

-- | @Selector@ for @readAttributeSensitivityMaxWithParams:@
readAttributeSensitivityMaxWithParamsSelector :: Selector
readAttributeSensitivityMaxWithParamsSelector = mkSelector "readAttributeSensitivityMaxWithParams:"

-- | @Selector@ for @readAttributeSensitivityWithParams:@
readAttributeSensitivityWithParamsSelector :: Selector
readAttributeSensitivityWithParamsSelector = mkSelector "readAttributeSensitivityWithParams:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:expectedValueInterval:@
writeAttributeSensitivityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSensitivityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithParams:@
readAttributeTwoDCartesianMaxWithParamsSelector :: Selector
readAttributeTwoDCartesianMaxWithParamsSelector = mkSelector "readAttributeTwoDCartesianMaxWithParams:"

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

