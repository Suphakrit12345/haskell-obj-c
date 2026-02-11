{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ethernet Network Diagnostics    The Ethernet Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRClusterEthernetNetworkDiagnostics@.
module ObjC.Matter.MTRClusterEthernetNetworkDiagnostics
  ( MTRClusterEthernetNetworkDiagnostics
  , IsMTRClusterEthernetNetworkDiagnostics(..)
  , resetCountsWithParams_expectedValues_expectedValueInterval_completion
  , resetCountsWithExpectedValues_expectedValueInterval_completion
  , readAttributePHYRateWithParams
  , readAttributeFullDuplexWithParams
  , readAttributePacketRxCountWithParams
  , readAttributePacketTxCountWithParams
  , readAttributeTxErrCountWithParams
  , readAttributeCollisionCountWithParams
  , readAttributeOverrunCountWithParams
  , readAttributeCarrierDetectWithParams
  , readAttributeTimeSinceResetWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributePHYRateWithParamsSelector
  , readAttributeFullDuplexWithParamsSelector
  , readAttributePacketRxCountWithParamsSelector
  , readAttributePacketTxCountWithParamsSelector
  , readAttributeTxErrCountWithParamsSelector
  , readAttributeCollisionCountWithParamsSelector
  , readAttributeOverrunCountWithParamsSelector
  , readAttributeCarrierDetectWithParamsSelector
  , readAttributeTimeSinceResetWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEthernetNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completion mtrClusterEthernetNetworkDiagnostics  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePHYRateWithParams:@
readAttributePHYRateWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePHYRateWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributePHYRateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFullDuplexWithParams:@
readAttributeFullDuplexWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFullDuplexWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFullDuplexWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketRxCountWithParams:@
readAttributePacketRxCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketRxCountWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketRxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketTxCountWithParams:@
readAttributePacketTxCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketTxCountWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketTxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxErrCountWithParams:@
readAttributeTxErrCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrCountWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTxErrCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCollisionCountWithParams:@
readAttributeCollisionCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCollisionCountWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCollisionCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOverrunCountWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCarrierDetectWithParams:@
readAttributeCarrierDetectWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCarrierDetectWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCarrierDetectWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeSinceResetWithParams:@
readAttributeTimeSinceResetWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTimeSinceResetWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTimeSinceResetWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEthernetNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics => mtrClusterEthernetNetworkDiagnostics -> IO (Id MTRClusterEthernetNetworkDiagnostics)
init_ mtrClusterEthernetNetworkDiagnostics  =
    sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterEthernetNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEthernetNetworkDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterEthernetNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpoint_queue mtrClusterEthernetNetworkDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterEthernetNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterEthernetNetworkDiagnostics  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEthernetNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpointID_queue mtrClusterEthernetNetworkDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterEthernetNetworkDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePHYRateWithParams:@
readAttributePHYRateWithParamsSelector :: Selector
readAttributePHYRateWithParamsSelector = mkSelector "readAttributePHYRateWithParams:"

-- | @Selector@ for @readAttributeFullDuplexWithParams:@
readAttributeFullDuplexWithParamsSelector :: Selector
readAttributeFullDuplexWithParamsSelector = mkSelector "readAttributeFullDuplexWithParams:"

-- | @Selector@ for @readAttributePacketRxCountWithParams:@
readAttributePacketRxCountWithParamsSelector :: Selector
readAttributePacketRxCountWithParamsSelector = mkSelector "readAttributePacketRxCountWithParams:"

-- | @Selector@ for @readAttributePacketTxCountWithParams:@
readAttributePacketTxCountWithParamsSelector :: Selector
readAttributePacketTxCountWithParamsSelector = mkSelector "readAttributePacketTxCountWithParams:"

-- | @Selector@ for @readAttributeTxErrCountWithParams:@
readAttributeTxErrCountWithParamsSelector :: Selector
readAttributeTxErrCountWithParamsSelector = mkSelector "readAttributeTxErrCountWithParams:"

-- | @Selector@ for @readAttributeCollisionCountWithParams:@
readAttributeCollisionCountWithParamsSelector :: Selector
readAttributeCollisionCountWithParamsSelector = mkSelector "readAttributeCollisionCountWithParams:"

-- | @Selector@ for @readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParamsSelector :: Selector
readAttributeOverrunCountWithParamsSelector = mkSelector "readAttributeOverrunCountWithParams:"

-- | @Selector@ for @readAttributeCarrierDetectWithParams:@
readAttributeCarrierDetectWithParamsSelector :: Selector
readAttributeCarrierDetectWithParamsSelector = mkSelector "readAttributeCarrierDetectWithParams:"

-- | @Selector@ for @readAttributeTimeSinceResetWithParams:@
readAttributeTimeSinceResetWithParamsSelector :: Selector
readAttributeTimeSinceResetWithParamsSelector = mkSelector "readAttributeTimeSinceResetWithParams:"

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

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

