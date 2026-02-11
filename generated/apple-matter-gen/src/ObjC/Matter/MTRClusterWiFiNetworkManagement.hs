{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wi-Fi Network Management    Functionality to retrieve operational information about a managed Wi-Fi network.
--
-- Generated bindings for @MTRClusterWiFiNetworkManagement@.
module ObjC.Matter.MTRClusterWiFiNetworkManagement
  ( MTRClusterWiFiNetworkManagement
  , IsMTRClusterWiFiNetworkManagement(..)
  , networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion
  , networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeSSIDWithParams
  , readAttributePassphraseSurrogateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeSSIDWithParamsSelector
  , readAttributePassphraseSurrogateWithParamsSelector
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

-- | @- networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRWiFiNetworkManagementClusterNetworkPassphraseRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWiFiNetworkManagement (mkSelector "networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWiFiNetworkManagement (mkSelector "networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSSIDWithParams:@
readAttributeSSIDWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeSSIDWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeSSIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePassphraseSurrogateWithParams:@
readAttributePassphraseSurrogateWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributePassphraseSurrogateWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributePassphraseSurrogateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWiFiNetworkManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement => mtrClusterWiFiNetworkManagement -> IO (Id MTRClusterWiFiNetworkManagement)
init_ mtrClusterWiFiNetworkManagement  =
    sendMsg mtrClusterWiFiNetworkManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWiFiNetworkManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWiFiNetworkManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWiFiNetworkManagement -> device -> endpointID -> queue -> IO (Id MTRClusterWiFiNetworkManagement)
initWithDevice_endpointID_queue mtrClusterWiFiNetworkManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWiFiNetworkManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSSIDWithParams:@
readAttributeSSIDWithParamsSelector :: Selector
readAttributeSSIDWithParamsSelector = mkSelector "readAttributeSSIDWithParams:"

-- | @Selector@ for @readAttributePassphraseSurrogateWithParams:@
readAttributePassphraseSurrogateWithParamsSelector :: Selector
readAttributePassphraseSurrogateWithParamsSelector = mkSelector "readAttributePassphraseSurrogateWithParams:"

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

