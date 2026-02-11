{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groups    Attributes and commands for group configuration and manipulation.
--
-- Generated bindings for @MTRClusterGroups@.
module ObjC.Matter.MTRClusterGroups
  ( MTRClusterGroups
  , IsMTRClusterGroups(..)
  , addGroupWithParams_expectedValues_expectedValueInterval_completion
  , viewGroupWithParams_expectedValues_expectedValueInterval_completion
  , getGroupMembershipWithParams_expectedValues_expectedValueInterval_completion
  , removeGroupWithParams_expectedValues_expectedValueInterval_completion
  , removeAllGroupsWithParams_expectedValues_expectedValueInterval_completion
  , removeAllGroupsWithExpectedValues_expectedValueInterval_completion
  , addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeNameSupportWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , addGroupWithParams_expectedValues_expectedValueInterval_completionHandler
  , viewGroupWithParams_expectedValues_expectedValueInterval_completionHandler
  , getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeGroupWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandler
  , addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , addGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , viewGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAllGroupsWithExpectedValues_expectedValueInterval_completionSelector
  , addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeNameSupportWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , addGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , viewGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterAddGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- viewGroupWithParams:expectedValues:expectedValueInterval:completion:@
viewGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterViewGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
viewGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "viewGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getGroupMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterGetGroupMembershipParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "getGroupMembershipWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterRemoveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeAllGroupsWithParams:expectedValues:expectedValueInterval:completion:@
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterRemoveAllGroupsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "removeAllGroupsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeAllGroupsWithExpectedValues:expectedValueInterval:completion:@
removeAllGroupsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllGroupsWithExpectedValues_expectedValueInterval_completion mtrClusterGroups  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGroups (mkSelector "removeAllGroupsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterAddGroupIfIdentifyingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNameSupportWithParams:@
readAttributeNameSupportWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeNameSupportWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeNameSupportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGroups mtrClusterGroups, IsMTRReadParams params) => mtrClusterGroups -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGroups  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroups (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterGroups mtrClusterGroups => mtrClusterGroups -> IO (Id MTRClusterGroups)
init_ mtrClusterGroups  =
    sendMsg mtrClusterGroups (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterGroups)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGroups"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGroups mtrClusterGroups, IsMTRDevice device, IsNSObject queue) => mtrClusterGroups -> device -> CUShort -> queue -> IO (Id MTRClusterGroups)
initWithDevice_endpoint_queue mtrClusterGroups  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterGroups (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- addGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
addGroupWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterAddGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- viewGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
viewGroupWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterViewGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
viewGroupWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "viewGroupWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getGroupMembershipWithParams:expectedValues:expectedValueInterval:completionHandler:@
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterGetGroupMembershipParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "getGroupMembershipWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeGroupWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterRemoveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeAllGroupsWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterRemoveAllGroupsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "removeAllGroupsWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeAllGroupsWithExpectedValues:expectedValueInterval:completionHandler:@
removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterGroups  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGroups (mkSelector "removeAllGroupsWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completionHandler:@
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroups mtrClusterGroups, IsMTRGroupsClusterAddGroupIfIdentifyingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroups -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroups  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroups (mkSelector "addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGroups mtrClusterGroups, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGroups -> device -> endpointID -> queue -> IO (Id MTRClusterGroups)
initWithDevice_endpointID_queue mtrClusterGroups  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterGroups (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @viewGroupWithParams:expectedValues:expectedValueInterval:completion:@
viewGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
viewGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "viewGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getGroupMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getGroupMembershipWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAllGroupsWithParams:expectedValues:expectedValueInterval:completion:@
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeAllGroupsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAllGroupsWithExpectedValues:expectedValueInterval:completion:@
removeAllGroupsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
removeAllGroupsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "removeAllGroupsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeNameSupportWithParams:@
readAttributeNameSupportWithParamsSelector :: Selector
readAttributeNameSupportWithParamsSelector = mkSelector "readAttributeNameSupportWithParams:"

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

-- | @Selector@ for @addGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
addGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @viewGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
viewGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
viewGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "viewGroupWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getGroupMembershipWithParams:expectedValues:expectedValueInterval:completionHandler:@
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getGroupMembershipWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getGroupMembershipWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeGroupWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
removeGroupWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeAllGroupsWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
removeAllGroupsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeAllGroupsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeAllGroupsWithExpectedValues:expectedValueInterval:completionHandler:@
removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
removeAllGroupsWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeAllGroupsWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completionHandler:@
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addGroupIfIdentifyingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addGroupIfIdentifyingWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

