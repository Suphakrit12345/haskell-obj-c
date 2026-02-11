{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Border Router Management    Manage the Thread network of Thread Border Router
--
-- Generated bindings for @MTRClusterThreadBorderRouterManagement@.
module ObjC.Matter.MTRClusterThreadBorderRouterManagement
  ( MTRClusterThreadBorderRouterManagement
  , IsMTRClusterThreadBorderRouterManagement(..)
  , getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion
  , getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion
  , setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeBorderRouterNameWithParams
  , readAttributeBorderAgentIDWithParams
  , readAttributeThreadVersionWithParams
  , readAttributeInterfaceEnabledWithParams
  , readAttributeActiveDatasetTimestampWithParams
  , readAttributePendingDatasetTimestampWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector
  , getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector
  , setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeBorderRouterNameWithParamsSelector
  , readAttributeBorderAgentIDWithParamsSelector
  , readAttributeThreadVersionWithParamsSelector
  , readAttributeInterfaceEnabledWithParamsSelector
  , readAttributeActiveDatasetTimestampWithParamsSelector
  , readAttributePendingDatasetTimestampWithParamsSelector
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

-- | @- getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBorderRouterNameWithParams:@
readAttributeBorderRouterNameWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeBorderRouterNameWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeBorderRouterNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBorderAgentIDWithParams:@
readAttributeBorderAgentIDWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeBorderAgentIDWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeBorderAgentIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeThreadVersionWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeThreadVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeInterfaceEnabledWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeInterfaceEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveDatasetTimestampWithParams:@
readAttributeActiveDatasetTimestampWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeActiveDatasetTimestampWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeActiveDatasetTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePendingDatasetTimestampWithParams:@
readAttributePendingDatasetTimestampWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributePendingDatasetTimestampWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributePendingDatasetTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadBorderRouterManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement => mtrClusterThreadBorderRouterManagement -> IO (Id MTRClusterThreadBorderRouterManagement)
init_ mtrClusterThreadBorderRouterManagement  =
    sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterThreadBorderRouterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadBorderRouterManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadBorderRouterManagement -> device -> endpointID -> queue -> IO (Id MTRClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queue mtrClusterThreadBorderRouterManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterThreadBorderRouterManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBorderRouterNameWithParams:@
readAttributeBorderRouterNameWithParamsSelector :: Selector
readAttributeBorderRouterNameWithParamsSelector = mkSelector "readAttributeBorderRouterNameWithParams:"

-- | @Selector@ for @readAttributeBorderAgentIDWithParams:@
readAttributeBorderAgentIDWithParamsSelector :: Selector
readAttributeBorderAgentIDWithParamsSelector = mkSelector "readAttributeBorderAgentIDWithParams:"

-- | @Selector@ for @readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParamsSelector :: Selector
readAttributeThreadVersionWithParamsSelector = mkSelector "readAttributeThreadVersionWithParams:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParamsSelector :: Selector
readAttributeInterfaceEnabledWithParamsSelector = mkSelector "readAttributeInterfaceEnabledWithParams:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithParams:@
readAttributeActiveDatasetTimestampWithParamsSelector :: Selector
readAttributeActiveDatasetTimestampWithParamsSelector = mkSelector "readAttributeActiveDatasetTimestampWithParams:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithParams:@
readAttributePendingDatasetTimestampWithParamsSelector :: Selector
readAttributePendingDatasetTimestampWithParamsSelector = mkSelector "readAttributePendingDatasetTimestampWithParams:"

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

