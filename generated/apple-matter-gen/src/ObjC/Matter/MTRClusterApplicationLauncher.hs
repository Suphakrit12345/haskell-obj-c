{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Launcher    This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterApplicationLauncher@.
module ObjC.Matter.MTRClusterApplicationLauncher
  ( MTRClusterApplicationLauncher
  , IsMTRClusterApplicationLauncher(..)
  , launchAppWithParams_expectedValues_expectedValueInterval_completion
  , launchAppWithExpectedValues_expectedValueInterval_completion
  , stopAppWithParams_expectedValues_expectedValueInterval_completion
  , stopAppWithExpectedValues_expectedValueInterval_completion
  , hideAppWithParams_expectedValues_expectedValueInterval_completion
  , hideAppWithExpectedValues_expectedValueInterval_completion
  , readAttributeCatalogListWithParams
  , readAttributeCurrentAppWithParams
  , writeAttributeCurrentAppWithValue_expectedValueInterval
  , writeAttributeCurrentAppWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , launchAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , hideAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , launchAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , launchAppWithExpectedValues_expectedValueInterval_completionSelector
  , stopAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopAppWithExpectedValues_expectedValueInterval_completionSelector
  , hideAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , hideAppWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeCatalogListWithParamsSelector
  , readAttributeCurrentAppWithParamsSelector
  , writeAttributeCurrentAppWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- launchAppWithParams:expectedValues:expectedValueInterval:completion:@
launchAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- launchAppWithExpectedValues:expectedValueInterval:completion:@
launchAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterApplicationLauncher (mkSelector "launchAppWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopAppWithParams:expectedValues:expectedValueInterval:completion:@
stopAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopAppWithExpectedValues:expectedValueInterval:completion:@
stopAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterApplicationLauncher (mkSelector "stopAppWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- hideAppWithParams:expectedValues:expectedValueInterval:completion:@
hideAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- hideAppWithExpectedValues:expectedValueInterval:completion:@
hideAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterApplicationLauncher (mkSelector "hideAppWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCatalogListWithParams:@
readAttributeCatalogListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeCatalogListWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeCatalogListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentAppWithParams:@
readAttributeCurrentAppWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeCurrentAppWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeCurrentAppWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeCurrentAppWithValue:expectedValueInterval:@
writeAttributeCurrentAppWithValue_expectedValueInterval :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentAppWithValue_expectedValueInterval mtrClusterApplicationLauncher  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeCurrentAppWithValue:expectedValueInterval:params:@
writeAttributeCurrentAppWithValue_expectedValueInterval_params :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterApplicationLauncher -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentAppWithValue_expectedValueInterval_params mtrClusterApplicationLauncher  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterApplicationLauncher  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterApplicationLauncher (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher => mtrClusterApplicationLauncher -> IO (Id MTRClusterApplicationLauncher)
init_ mtrClusterApplicationLauncher  =
    sendMsg mtrClusterApplicationLauncher (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterApplicationLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRClusterApplicationLauncher"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRDevice device, IsNSObject queue) => mtrClusterApplicationLauncher -> device -> CUShort -> queue -> IO (Id MTRClusterApplicationLauncher)
initWithDevice_endpoint_queue mtrClusterApplicationLauncher  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterApplicationLauncher (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterApplicationLauncher -> device -> endpointID -> queue -> IO (Id MTRClusterApplicationLauncher)
initWithDevice_endpointID_queue mtrClusterApplicationLauncher  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterApplicationLauncher (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchAppWithParams:expectedValues:expectedValueInterval:completion:@
launchAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
launchAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @launchAppWithExpectedValues:expectedValueInterval:completion:@
launchAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector
launchAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "launchAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopAppWithParams:expectedValues:expectedValueInterval:completion:@
stopAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopAppWithExpectedValues:expectedValueInterval:completion:@
stopAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector
stopAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideAppWithParams:expectedValues:expectedValueInterval:completion:@
hideAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
hideAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideAppWithExpectedValues:expectedValueInterval:completion:@
hideAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector
hideAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "hideAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCatalogListWithParams:@
readAttributeCatalogListWithParamsSelector :: Selector
readAttributeCatalogListWithParamsSelector = mkSelector "readAttributeCatalogListWithParams:"

-- | @Selector@ for @readAttributeCurrentAppWithParams:@
readAttributeCurrentAppWithParamsSelector :: Selector
readAttributeCurrentAppWithParamsSelector = mkSelector "readAttributeCurrentAppWithParams:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:expectedValueInterval:@
writeAttributeCurrentAppWithValue_expectedValueIntervalSelector :: Selector
writeAttributeCurrentAppWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:expectedValueInterval:params:@
writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

