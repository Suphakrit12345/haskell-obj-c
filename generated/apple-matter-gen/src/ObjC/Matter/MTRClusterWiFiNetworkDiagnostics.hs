{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wi-Fi Network Diagnostics    The Wi-Fi Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRClusterWiFiNetworkDiagnostics@.
module ObjC.Matter.MTRClusterWiFiNetworkDiagnostics
  ( MTRClusterWiFiNetworkDiagnostics
  , IsMTRClusterWiFiNetworkDiagnostics(..)
  , resetCountsWithParams_expectedValues_expectedValueInterval_completion
  , resetCountsWithExpectedValues_expectedValueInterval_completion
  , readAttributeBSSIDWithParams
  , readAttributeSecurityTypeWithParams
  , readAttributeWiFiVersionWithParams
  , readAttributeChannelNumberWithParams
  , readAttributeRSSIWithParams
  , readAttributeBeaconLostCountWithParams
  , readAttributeBeaconRxCountWithParams
  , readAttributePacketMulticastRxCountWithParams
  , readAttributePacketMulticastTxCountWithParams
  , readAttributePacketUnicastRxCountWithParams
  , readAttributePacketUnicastTxCountWithParams
  , readAttributeCurrentMaxRateWithParams
  , readAttributeOverrunCountWithParams
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
  , readAttributeBssidWithParams
  , readAttributeRssiWithParams
  , initWithDevice_endpointID_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeBSSIDWithParamsSelector
  , readAttributeSecurityTypeWithParamsSelector
  , readAttributeWiFiVersionWithParamsSelector
  , readAttributeChannelNumberWithParamsSelector
  , readAttributeRSSIWithParamsSelector
  , readAttributeBeaconLostCountWithParamsSelector
  , readAttributeBeaconRxCountWithParamsSelector
  , readAttributePacketMulticastRxCountWithParamsSelector
  , readAttributePacketMulticastTxCountWithParamsSelector
  , readAttributePacketUnicastRxCountWithParamsSelector
  , readAttributePacketUnicastTxCountWithParamsSelector
  , readAttributeCurrentMaxRateWithParamsSelector
  , readAttributeOverrunCountWithParamsSelector
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
  , readAttributeBssidWithParamsSelector
  , readAttributeRssiWithParamsSelector
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
resetCountsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRWiFiNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkDiagnostics  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBSSIDWithParams:@
readAttributeBSSIDWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBSSIDWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBSSIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSecurityTypeWithParams:@
readAttributeSecurityTypeWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeSecurityTypeWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeSecurityTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiFiVersionWithParams:@
readAttributeWiFiVersionWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeWiFiVersionWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeWiFiVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeChannelNumberWithParams:@
readAttributeChannelNumberWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChannelNumberWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeChannelNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRSSIWithParams:@
readAttributeRSSIWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRSSIWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeRSSIWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBeaconLostCountWithParams:@
readAttributeBeaconLostCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBeaconLostCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconLostCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBeaconRxCountWithParams:@
readAttributeBeaconRxCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBeaconRxCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconRxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketMulticastRxCountWithParams:@
readAttributePacketMulticastRxCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketMulticastRxCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastRxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketMulticastTxCountWithParams:@
readAttributePacketMulticastTxCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketMulticastTxCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastTxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketUnicastRxCountWithParams:@
readAttributePacketUnicastRxCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketUnicastRxCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastRxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePacketUnicastTxCountWithParams:@
readAttributePacketUnicastTxCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketUnicastTxCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastTxCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentMaxRateWithParams:@
readAttributeCurrentMaxRateWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCurrentMaxRateWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeCurrentMaxRateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOverrunCountWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics => mtrClusterWiFiNetworkDiagnostics -> IO (Id MTRClusterWiFiNetworkDiagnostics)
init_ mtrClusterWiFiNetworkDiagnostics  =
    sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWiFiNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWiFiNetworkDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterWiFiNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterWiFiNetworkDiagnostics)
initWithDevice_endpoint_queue mtrClusterWiFiNetworkDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRWiFiNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWiFiNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWiFiNetworkDiagnostics  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBssidWithParams:@
readAttributeBssidWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBssidWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBssidWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRssiWithParams:@
readAttributeRssiWithParams :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRReadParams params) => mtrClusterWiFiNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRssiWithParams mtrClusterWiFiNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "readAttributeRssiWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWiFiNetworkDiagnostics mtrClusterWiFiNetworkDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWiFiNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterWiFiNetworkDiagnostics)
initWithDevice_endpointID_queue mtrClusterWiFiNetworkDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWiFiNetworkDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBSSIDWithParams:@
readAttributeBSSIDWithParamsSelector :: Selector
readAttributeBSSIDWithParamsSelector = mkSelector "readAttributeBSSIDWithParams:"

-- | @Selector@ for @readAttributeSecurityTypeWithParams:@
readAttributeSecurityTypeWithParamsSelector :: Selector
readAttributeSecurityTypeWithParamsSelector = mkSelector "readAttributeSecurityTypeWithParams:"

-- | @Selector@ for @readAttributeWiFiVersionWithParams:@
readAttributeWiFiVersionWithParamsSelector :: Selector
readAttributeWiFiVersionWithParamsSelector = mkSelector "readAttributeWiFiVersionWithParams:"

-- | @Selector@ for @readAttributeChannelNumberWithParams:@
readAttributeChannelNumberWithParamsSelector :: Selector
readAttributeChannelNumberWithParamsSelector = mkSelector "readAttributeChannelNumberWithParams:"

-- | @Selector@ for @readAttributeRSSIWithParams:@
readAttributeRSSIWithParamsSelector :: Selector
readAttributeRSSIWithParamsSelector = mkSelector "readAttributeRSSIWithParams:"

-- | @Selector@ for @readAttributeBeaconLostCountWithParams:@
readAttributeBeaconLostCountWithParamsSelector :: Selector
readAttributeBeaconLostCountWithParamsSelector = mkSelector "readAttributeBeaconLostCountWithParams:"

-- | @Selector@ for @readAttributeBeaconRxCountWithParams:@
readAttributeBeaconRxCountWithParamsSelector :: Selector
readAttributeBeaconRxCountWithParamsSelector = mkSelector "readAttributeBeaconRxCountWithParams:"

-- | @Selector@ for @readAttributePacketMulticastRxCountWithParams:@
readAttributePacketMulticastRxCountWithParamsSelector :: Selector
readAttributePacketMulticastRxCountWithParamsSelector = mkSelector "readAttributePacketMulticastRxCountWithParams:"

-- | @Selector@ for @readAttributePacketMulticastTxCountWithParams:@
readAttributePacketMulticastTxCountWithParamsSelector :: Selector
readAttributePacketMulticastTxCountWithParamsSelector = mkSelector "readAttributePacketMulticastTxCountWithParams:"

-- | @Selector@ for @readAttributePacketUnicastRxCountWithParams:@
readAttributePacketUnicastRxCountWithParamsSelector :: Selector
readAttributePacketUnicastRxCountWithParamsSelector = mkSelector "readAttributePacketUnicastRxCountWithParams:"

-- | @Selector@ for @readAttributePacketUnicastTxCountWithParams:@
readAttributePacketUnicastTxCountWithParamsSelector :: Selector
readAttributePacketUnicastTxCountWithParamsSelector = mkSelector "readAttributePacketUnicastTxCountWithParams:"

-- | @Selector@ for @readAttributeCurrentMaxRateWithParams:@
readAttributeCurrentMaxRateWithParamsSelector :: Selector
readAttributeCurrentMaxRateWithParamsSelector = mkSelector "readAttributeCurrentMaxRateWithParams:"

-- | @Selector@ for @readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParamsSelector :: Selector
readAttributeOverrunCountWithParamsSelector = mkSelector "readAttributeOverrunCountWithParams:"

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

-- | @Selector@ for @readAttributeBssidWithParams:@
readAttributeBssidWithParamsSelector :: Selector
readAttributeBssidWithParamsSelector = mkSelector "readAttributeBssidWithParams:"

-- | @Selector@ for @readAttributeRssiWithParams:@
readAttributeRssiWithParamsSelector :: Selector
readAttributeRssiWithParamsSelector = mkSelector "readAttributeRssiWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

