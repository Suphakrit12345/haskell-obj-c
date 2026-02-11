{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Group Key Management    The Group Key Management Cluster is the mechanism by which group keys are managed.
--
-- Generated bindings for @MTRClusterGroupKeyManagement@.
module ObjC.Matter.MTRClusterGroupKeyManagement
  ( MTRClusterGroupKeyManagement
  , IsMTRClusterGroupKeyManagement(..)
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadWithParams_expectedValues_expectedValueInterval_completion
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion
  , readAttributeGroupKeyMapWithParams
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval_params
  , readAttributeGroupTableWithParams
  , readAttributeMaxGroupsPerFabricWithParams
  , readAttributeMaxGroupKeysPerFabricWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeGroupKeyMapWithParamsSelector
  , writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector
  , readAttributeGroupTableWithParamsSelector
  , readAttributeMaxGroupsPerFabricWithParamsSelector
  , readAttributeMaxGroupKeysPerFabricWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- keySetWriteWithParams:expectedValues:expectedValueInterval:completion:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- keySetReadWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGroupKeyMapWithParams:@
readAttributeGroupKeyMapWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGroupKeyMapWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeGroupKeyMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeGroupKeyMapWithValue:expectedValueInterval:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeGroupKeyMapWithValue_expectedValueInterval mtrClusterGroupKeyManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval_params :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterGroupKeyManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeGroupKeyMapWithValue_expectedValueInterval_params mtrClusterGroupKeyManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGroupTableWithParams:@
readAttributeGroupTableWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGroupTableWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeGroupTableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxGroupsPerFabricWithParams:@
readAttributeMaxGroupsPerFabricWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeMaxGroupsPerFabricWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupsPerFabricWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxGroupKeysPerFabricWithParams:@
readAttributeMaxGroupKeysPerFabricWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeMaxGroupKeysPerFabricWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupKeysPerFabricWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGroupKeyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupKeyManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement => mtrClusterGroupKeyManagement -> IO (Id MTRClusterGroupKeyManagement)
init_ mtrClusterGroupKeyManagement  =
    sendMsg mtrClusterGroupKeyManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterGroupKeyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGroupKeyManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRDevice device, IsNSObject queue) => mtrClusterGroupKeyManagement -> device -> CUShort -> queue -> IO (Id MTRClusterGroupKeyManagement)
initWithDevice_endpoint_queue mtrClusterGroupKeyManagement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterGroupKeyManagement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGroupKeyManagement -> device -> endpointID -> queue -> IO (Id MTRClusterGroupKeyManagement)
initWithDevice_endpointID_queue mtrClusterGroupKeyManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterGroupKeyManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keySetWriteWithParams:expectedValues:expectedValueInterval:completion:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector :: Selector
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:@
readAttributeGroupKeyMapWithParamsSelector :: Selector
readAttributeGroupKeyMapWithParamsSelector = mkSelector "readAttributeGroupKeyMapWithParams:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:expectedValueInterval:@
writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector :: Selector
writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeGroupTableWithParams:@
readAttributeGroupTableWithParamsSelector :: Selector
readAttributeGroupTableWithParamsSelector = mkSelector "readAttributeGroupTableWithParams:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithParams:@
readAttributeMaxGroupsPerFabricWithParamsSelector :: Selector
readAttributeMaxGroupsPerFabricWithParamsSelector = mkSelector "readAttributeMaxGroupsPerFabricWithParams:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithParams:@
readAttributeMaxGroupKeysPerFabricWithParamsSelector :: Selector
readAttributeMaxGroupKeysPerFabricWithParamsSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithParams:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

