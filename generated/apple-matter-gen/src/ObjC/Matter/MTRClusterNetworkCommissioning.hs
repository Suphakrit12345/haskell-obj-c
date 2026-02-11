{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Network Commissioning    Functionality to configure, enable, disable network credentials and access on a Matter device.
--
-- Generated bindings for @MTRClusterNetworkCommissioning@.
module ObjC.Matter.MTRClusterNetworkCommissioning
  ( MTRClusterNetworkCommissioning
  , IsMTRClusterNetworkCommissioning(..)
  , scanNetworksWithParams_expectedValues_expectedValueInterval_completion
  , scanNetworksWithExpectedValues_expectedValueInterval_completion
  , addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completion
  , addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completion
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completion
  , connectNetworkWithParams_expectedValues_expectedValueInterval_completion
  , reorderNetworkWithParams_expectedValues_expectedValueInterval_completion
  , queryIdentityWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxNetworksWithParams
  , readAttributeNetworksWithParams
  , readAttributeScanMaxTimeSecondsWithParams
  , readAttributeConnectMaxTimeSecondsWithParams
  , readAttributeInterfaceEnabledWithParams
  , writeAttributeInterfaceEnabledWithValue_expectedValueInterval
  , writeAttributeInterfaceEnabledWithValue_expectedValueInterval_params
  , readAttributeLastNetworkingStatusWithParams
  , readAttributeLastNetworkIDWithParams
  , readAttributeLastConnectErrorValueWithParams
  , readAttributeSupportedWiFiBandsWithParams
  , readAttributeSupportedThreadFeaturesWithParams
  , readAttributeThreadVersionWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandler
  , addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandler
  , addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandler
  , connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandler
  , reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , scanNetworksWithParams_expectedValues_expectedValueInterval_completionSelector
  , scanNetworksWithExpectedValues_expectedValueInterval_completionSelector
  , addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , connectNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , reorderNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , queryIdentityWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaxNetworksWithParamsSelector
  , readAttributeNetworksWithParamsSelector
  , readAttributeScanMaxTimeSecondsWithParamsSelector
  , readAttributeConnectMaxTimeSecondsWithParamsSelector
  , readAttributeInterfaceEnabledWithParamsSelector
  , writeAttributeInterfaceEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeInterfaceEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeLastNetworkingStatusWithParamsSelector
  , readAttributeLastNetworkIDWithParamsSelector
  , readAttributeLastConnectErrorValueWithParamsSelector
  , readAttributeSupportedWiFiBandsWithParamsSelector
  , readAttributeSupportedThreadFeaturesWithParamsSelector
  , readAttributeThreadVersionWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- scanNetworksWithParams:expectedValues:expectedValueInterval:completion:@
scanNetworksWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterScanNetworksParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
scanNetworksWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "scanNetworksWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- scanNetworksWithExpectedValues:expectedValueInterval:completion:@
scanNetworksWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
scanNetworksWithExpectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterNetworkCommissioning (mkSelector "scanNetworksWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterRemoveNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- connectNetworkWithParams:expectedValues:expectedValueInterval:completion:@
connectNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterConnectNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
connectNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "connectNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- reorderNetworkWithParams:expectedValues:expectedValueInterval:completion:@
reorderNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterReorderNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reorderNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "reorderNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- queryIdentityWithParams:expectedValues:expectedValueInterval:completion:@
queryIdentityWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterQueryIdentityParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
queryIdentityWithParams_expectedValues_expectedValueInterval_completion mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "queryIdentityWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxNetworksWithParams:@
readAttributeMaxNetworksWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeMaxNetworksWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeMaxNetworksWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNetworksWithParams:@
readAttributeNetworksWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeNetworksWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeNetworksWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScanMaxTimeSecondsWithParams:@
readAttributeScanMaxTimeSecondsWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeScanMaxTimeSecondsWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeScanMaxTimeSecondsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeConnectMaxTimeSecondsWithParams:@
readAttributeConnectMaxTimeSecondsWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeConnectMaxTimeSecondsWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeConnectMaxTimeSecondsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeInterfaceEnabledWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeInterfaceEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeInterfaceEnabledWithValue:expectedValueInterval:@
writeAttributeInterfaceEnabledWithValue_expectedValueInterval :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeInterfaceEnabledWithValue_expectedValueInterval mtrClusterNetworkCommissioning  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeInterfaceEnabledWithValue:expectedValueInterval:params:@
writeAttributeInterfaceEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterNetworkCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeInterfaceEnabledWithValue_expectedValueInterval_params mtrClusterNetworkCommissioning  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLastNetworkingStatusWithParams:@
readAttributeLastNetworkingStatusWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeLastNetworkingStatusWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkingStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLastNetworkIDWithParams:@
readAttributeLastNetworkIDWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeLastNetworkIDWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLastConnectErrorValueWithParams:@
readAttributeLastConnectErrorValueWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeLastConnectErrorValueWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeLastConnectErrorValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedWiFiBandsWithParams:@
readAttributeSupportedWiFiBandsWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeSupportedWiFiBandsWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeSupportedWiFiBandsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedThreadFeaturesWithParams:@
readAttributeSupportedThreadFeaturesWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeSupportedThreadFeaturesWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeSupportedThreadFeaturesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeThreadVersionWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeThreadVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRReadParams params) => mtrClusterNetworkCommissioning -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterNetworkCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterNetworkCommissioning (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning => mtrClusterNetworkCommissioning -> IO (Id MTRClusterNetworkCommissioning)
init_ mtrClusterNetworkCommissioning  =
    sendMsg mtrClusterNetworkCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterNetworkCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRClusterNetworkCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRDevice device, IsNSObject queue) => mtrClusterNetworkCommissioning -> device -> CUShort -> queue -> IO (Id MTRClusterNetworkCommissioning)
initWithDevice_endpoint_queue mtrClusterNetworkCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterNetworkCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- scanNetworksWithParams:expectedValues:expectedValueInterval:completionHandler:@
scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterScanNetworksParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "scanNetworksWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterRemoveNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- connectNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterConnectNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "connectNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- reorderNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterReorderNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterNetworkCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterNetworkCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "reorderNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterNetworkCommissioning mtrClusterNetworkCommissioning, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterNetworkCommissioning -> device -> endpointID -> queue -> IO (Id MTRClusterNetworkCommissioning)
initWithDevice_endpointID_queue mtrClusterNetworkCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterNetworkCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scanNetworksWithParams:expectedValues:expectedValueInterval:completion:@
scanNetworksWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
scanNetworksWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "scanNetworksWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @scanNetworksWithExpectedValues:expectedValueInterval:completion:@
scanNetworksWithExpectedValues_expectedValueInterval_completionSelector :: Selector
scanNetworksWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "scanNetworksWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @connectNetworkWithParams:expectedValues:expectedValueInterval:completion:@
connectNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
connectNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "connectNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @reorderNetworkWithParams:expectedValues:expectedValueInterval:completion:@
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "reorderNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @queryIdentityWithParams:expectedValues:expectedValueInterval:completion:@
queryIdentityWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
queryIdentityWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "queryIdentityWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxNetworksWithParams:@
readAttributeMaxNetworksWithParamsSelector :: Selector
readAttributeMaxNetworksWithParamsSelector = mkSelector "readAttributeMaxNetworksWithParams:"

-- | @Selector@ for @readAttributeNetworksWithParams:@
readAttributeNetworksWithParamsSelector :: Selector
readAttributeNetworksWithParamsSelector = mkSelector "readAttributeNetworksWithParams:"

-- | @Selector@ for @readAttributeScanMaxTimeSecondsWithParams:@
readAttributeScanMaxTimeSecondsWithParamsSelector :: Selector
readAttributeScanMaxTimeSecondsWithParamsSelector = mkSelector "readAttributeScanMaxTimeSecondsWithParams:"

-- | @Selector@ for @readAttributeConnectMaxTimeSecondsWithParams:@
readAttributeConnectMaxTimeSecondsWithParamsSelector :: Selector
readAttributeConnectMaxTimeSecondsWithParamsSelector = mkSelector "readAttributeConnectMaxTimeSecondsWithParams:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParamsSelector :: Selector
readAttributeInterfaceEnabledWithParamsSelector = mkSelector "readAttributeInterfaceEnabledWithParams:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:expectedValueInterval:@
writeAttributeInterfaceEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeInterfaceEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:expectedValueInterval:params:@
writeAttributeInterfaceEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeInterfaceEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLastNetworkingStatusWithParams:@
readAttributeLastNetworkingStatusWithParamsSelector :: Selector
readAttributeLastNetworkingStatusWithParamsSelector = mkSelector "readAttributeLastNetworkingStatusWithParams:"

-- | @Selector@ for @readAttributeLastNetworkIDWithParams:@
readAttributeLastNetworkIDWithParamsSelector :: Selector
readAttributeLastNetworkIDWithParamsSelector = mkSelector "readAttributeLastNetworkIDWithParams:"

-- | @Selector@ for @readAttributeLastConnectErrorValueWithParams:@
readAttributeLastConnectErrorValueWithParamsSelector :: Selector
readAttributeLastConnectErrorValueWithParamsSelector = mkSelector "readAttributeLastConnectErrorValueWithParams:"

-- | @Selector@ for @readAttributeSupportedWiFiBandsWithParams:@
readAttributeSupportedWiFiBandsWithParamsSelector :: Selector
readAttributeSupportedWiFiBandsWithParamsSelector = mkSelector "readAttributeSupportedWiFiBandsWithParams:"

-- | @Selector@ for @readAttributeSupportedThreadFeaturesWithParams:@
readAttributeSupportedThreadFeaturesWithParamsSelector :: Selector
readAttributeSupportedThreadFeaturesWithParamsSelector = mkSelector "readAttributeSupportedThreadFeaturesWithParams:"

-- | @Selector@ for @readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParamsSelector :: Selector
readAttributeThreadVersionWithParamsSelector = mkSelector "readAttributeThreadVersionWithParams:"

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

-- | @Selector@ for @scanNetworksWithParams:expectedValues:expectedValueInterval:completionHandler:@
scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
scanNetworksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "scanNetworksWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addOrUpdateWiFiNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addOrUpdateWiFiNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addOrUpdateThreadNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addOrUpdateThreadNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
removeNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @connectNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
connectNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "connectNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @reorderNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:@
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
reorderNetworkWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "reorderNetworkWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

