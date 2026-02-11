{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groupcast    The Groupcast cluster manages the content of the node-wide multicast Group membership that is part of the underlying interaction layer.
--
-- Generated bindings for @MTRClusterGroupcast@.
module ObjC.Matter.MTRClusterGroupcast
  ( MTRClusterGroupcast
  , IsMTRClusterGroupcast(..)
  , joinGroupWithParams_expectedValues_expectedValueInterval_completion
  , leaveGroupWithParams_expectedValues_expectedValueInterval_completion
  , updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion
  , expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion
  , configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMembershipWithParams
  , readAttributeMaxMembershipCountWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector
  , expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector
  , configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMembershipWithParamsSelector
  , readAttributeMaxMembershipCountWithParamsSelector
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

-- | @- joinGroupWithParams:expectedValues:expectedValueInterval:completion:@
joinGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterJoinGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
joinGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupcast (mkSelector "joinGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- leaveGroupWithParams:expectedValues:expectedValueInterval:completion:@
leaveGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterLeaveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
leaveGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupcast (mkSelector "leaveGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterUpdateGroupKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupcast (mkSelector "updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:@
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterExpireGracePeriodParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupcast (mkSelector "expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:@
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterConfigureAuxiliaryACLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGroupcast (mkSelector "configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMembershipWithParams:@
readAttributeMembershipWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeMembershipWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeMembershipWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMembershipCountWithParams:@
readAttributeMaxMembershipCountWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeMaxMembershipCountWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeMaxMembershipCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGroupcast  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGroupcast (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterGroupcast mtrClusterGroupcast => mtrClusterGroupcast -> IO (Id MTRClusterGroupcast)
init_ mtrClusterGroupcast  =
    sendMsg mtrClusterGroupcast (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterGroupcast)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGroupcast"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGroupcast -> device -> endpointID -> queue -> IO (Id MTRClusterGroupcast)
initWithDevice_endpointID_queue mtrClusterGroupcast  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterGroupcast (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @joinGroupWithParams:expectedValues:expectedValueInterval:completion:@
joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "joinGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @leaveGroupWithParams:expectedValues:expectedValueInterval:completion:@
leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "leaveGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:@
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:@
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMembershipWithParams:@
readAttributeMembershipWithParamsSelector :: Selector
readAttributeMembershipWithParamsSelector = mkSelector "readAttributeMembershipWithParams:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithParams:@
readAttributeMaxMembershipCountWithParamsSelector :: Selector
readAttributeMaxMembershipCountWithParamsSelector = mkSelector "readAttributeMaxMembershipCountWithParams:"

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

