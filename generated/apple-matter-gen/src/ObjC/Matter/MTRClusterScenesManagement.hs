{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Scenes Management    Attributes and commands for scene configuration and manipulation.
--
-- Generated bindings for @MTRClusterScenesManagement@.
module ObjC.Matter.MTRClusterScenesManagement
  ( MTRClusterScenesManagement
  , IsMTRClusterScenesManagement(..)
  , addSceneWithParams_expectedValues_expectedValueInterval_completion
  , viewSceneWithParams_expectedValues_expectedValueInterval_completion
  , removeSceneWithParams_expectedValues_expectedValueInterval_completion
  , removeAllScenesWithParams_expectedValues_expectedValueInterval_completion
  , storeSceneWithParams_expectedValues_expectedValueInterval_completion
  , recallSceneWithParams_expectedValues_expectedValueInterval_completion
  , getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion
  , copySceneWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSceneTableSizeWithParams
  , readAttributeFabricSceneInfoWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector
  , storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector
  , copySceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSceneTableSizeWithParamsSelector
  , readAttributeFabricSceneInfoWithParamsSelector
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

-- | @- addSceneWithParams:expectedValues:expectedValueInterval:completion:@
addSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterAddSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "addSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- viewSceneWithParams:expectedValues:expectedValueInterval:completion:@
viewSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterViewSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
viewSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "viewSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeSceneWithParams:expectedValues:expectedValueInterval:completion:@
removeSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRemoveSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "removeSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:@
removeAllScenesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRemoveAllScenesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllScenesWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- storeSceneWithParams:expectedValues:expectedValueInterval:completion:@
storeSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterStoreSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
storeSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "storeSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- recallSceneWithParams:expectedValues:expectedValueInterval:completion:@
recallSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRecallSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
recallSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "recallSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterGetSceneMembershipParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- copySceneWithParams:expectedValues:expectedValueInterval:completion:@
copySceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterCopySceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
copySceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterScenesManagement (mkSelector "copySceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSceneTableSizeWithParams:@
readAttributeSceneTableSizeWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeSceneTableSizeWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeSceneTableSizeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFabricSceneInfoWithParams:@
readAttributeFabricSceneInfoWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeFabricSceneInfoWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeFabricSceneInfoWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterScenesManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterScenesManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterScenesManagement mtrClusterScenesManagement => mtrClusterScenesManagement -> IO (Id MTRClusterScenesManagement)
init_ mtrClusterScenesManagement  =
    sendMsg mtrClusterScenesManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterScenesManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterScenesManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterScenesManagement -> device -> endpointID -> queue -> IO (Id MTRClusterScenesManagement)
initWithDevice_endpointID_queue mtrClusterScenesManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterScenesManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSceneWithParams:expectedValues:expectedValueInterval:completion:@
addSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @viewSceneWithParams:expectedValues:expectedValueInterval:completion:@
viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "viewSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeSceneWithParams:expectedValues:expectedValueInterval:completion:@
removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:@
removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @storeSceneWithParams:expectedValues:expectedValueInterval:completion:@
storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "storeSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @recallSceneWithParams:expectedValues:expectedValueInterval:completion:@
recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "recallSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @copySceneWithParams:expectedValues:expectedValueInterval:completion:@
copySceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
copySceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "copySceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSceneTableSizeWithParams:@
readAttributeSceneTableSizeWithParamsSelector :: Selector
readAttributeSceneTableSizeWithParamsSelector = mkSelector "readAttributeSceneTableSizeWithParams:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithParams:@
readAttributeFabricSceneInfoWithParamsSelector :: Selector
readAttributeFabricSceneInfoWithParamsSelector = mkSelector "readAttributeFabricSceneInfoWithParams:"

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

