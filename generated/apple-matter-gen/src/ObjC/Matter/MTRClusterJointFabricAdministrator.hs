{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Administrator    An instance of the Joint Fabric Administrator Cluster only applies to Joint Fabric Administrator nodes fulfilling the role of Anchor CA.
--
-- Generated bindings for @MTRClusterJointFabricAdministrator@.
module ObjC.Matter.MTRClusterJointFabricAdministrator
  ( MTRClusterJointFabricAdministrator
  , IsMTRClusterJointFabricAdministrator(..)
  , icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion
  , icaccsrRequestWithExpectedValues_expectedValueInterval_completion
  , addICACWithParams_expectedValues_expectedValueInterval_completion
  , openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorRequestWithExpectedValues_expectedValueInterval_completion
  , transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion
  , announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAdministratorFabricIndexWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector
  , addICACWithParams_expectedValues_expectedValueInterval_completionSelector
  , openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector
  , transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector
  , transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector
  , transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector
  , announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAdministratorFabricIndexWithParamsSelector
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

-- | @- ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterICACCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:@
icaccsrRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
icaccsrRequestWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterJointFabricAdministrator (mkSelector "ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addICACWithParams:expectedValues:expectedValueInterval:completion:@
addICACWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAddICACParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addICACWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "addICACWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorRequestWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterJointFabricAdministrator (mkSelector "transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterJointFabricAdministrator (mkSelector "transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:@
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAdministratorFabricIndexWithParams:@
readAttributeAdministratorFabricIndexWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAdministratorFabricIndexWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeAdministratorFabricIndexWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterJointFabricAdministrator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricAdministrator (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator => mtrClusterJointFabricAdministrator -> IO (Id MTRClusterJointFabricAdministrator)
init_ mtrClusterJointFabricAdministrator  =
    sendMsg mtrClusterJointFabricAdministrator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterJointFabricAdministrator)
new  =
  do
    cls' <- getRequiredClass "MTRClusterJointFabricAdministrator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterJointFabricAdministrator -> device -> endpointID -> queue -> IO (Id MTRClusterJointFabricAdministrator)
initWithDevice_endpointID_queue mtrClusterJointFabricAdministrator  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterJointFabricAdministrator (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:@
icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addICACWithParams:expectedValues:expectedValueInterval:completion:@
addICACWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addICACWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addICACWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector :: Selector
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:@
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithParams:@
readAttributeAdministratorFabricIndexWithParamsSelector :: Selector
readAttributeAdministratorFabricIndexWithParamsSelector = mkSelector "readAttributeAdministratorFabricIndexWithParams:"

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

