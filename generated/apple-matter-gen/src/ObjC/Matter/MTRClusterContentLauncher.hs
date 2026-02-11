{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Launcher    This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterContentLauncher@.
module ObjC.Matter.MTRClusterContentLauncher
  ( MTRClusterContentLauncher
  , IsMTRClusterContentLauncher(..)
  , launchContentWithParams_expectedValues_expectedValueInterval_completion
  , launchURLWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAcceptHeaderWithParams
  , readAttributeSupportedStreamingProtocolsWithParams
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , launchContentWithParams_expectedValues_expectedValueInterval_completionHandler
  , launchURLWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , launchContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , launchURLWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptHeaderWithParamsSelector
  , readAttributeSupportedStreamingProtocolsWithParamsSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- launchContentWithParams:expectedValues:expectedValueInterval:completion:@
launchContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentLauncher (mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- launchURLWithParams:expectedValues:expectedValueInterval:completion:@
launchURLWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchURLWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentLauncher (mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptHeaderWithParams:@
readAttributeAcceptHeaderWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptHeaderWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeAcceptHeaderWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedStreamingProtocolsWithParams:@
readAttributeSupportedStreamingProtocolsWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeSupportedStreamingProtocolsWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeSupportedStreamingProtocolsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval mtrClusterContentLauncher  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentLauncher (mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterContentLauncher -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params mtrClusterContentLauncher  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterContentLauncher (mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentLauncher (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterContentLauncher mtrClusterContentLauncher => mtrClusterContentLauncher -> IO (Id MTRClusterContentLauncher)
init_ mtrClusterContentLauncher  =
    sendMsg mtrClusterContentLauncher (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterContentLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentLauncher"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRDevice device, IsNSObject queue) => mtrClusterContentLauncher -> device -> CUShort -> queue -> IO (Id MTRClusterContentLauncher)
initWithDevice_endpoint_queue mtrClusterContentLauncher  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterContentLauncher (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchContentWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchContentWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterContentLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentLauncher (mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchURLWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchURLWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterContentLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentLauncher (mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentLauncher -> device -> endpointID -> queue -> IO (Id MTRClusterContentLauncher)
initWithDevice_endpointID_queue mtrClusterContentLauncher  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterContentLauncher (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchContentWithParams:expectedValues:expectedValueInterval:completion:@
launchContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
launchContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @launchURLWithParams:expectedValues:expectedValueInterval:completion:@
launchURLWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
launchURLWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAcceptHeaderWithParams:@
readAttributeAcceptHeaderWithParamsSelector :: Selector
readAttributeAcceptHeaderWithParamsSelector = mkSelector "readAttributeAcceptHeaderWithParams:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithParams:@
readAttributeSupportedStreamingProtocolsWithParamsSelector :: Selector
readAttributeSupportedStreamingProtocolsWithParamsSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithParams:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

