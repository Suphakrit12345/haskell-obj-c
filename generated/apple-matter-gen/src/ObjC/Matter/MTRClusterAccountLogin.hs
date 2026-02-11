{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Account Login    This cluster provides commands that facilitate user account login on a Content App or a node. For example, a Content App running on a Video Player device, which is represented as an endpoint (see [TV Architecture]), can use this cluster to help make the user account on the Content App match the user account on the Client.
--
-- Generated bindings for @MTRClusterAccountLogin@.
module ObjC.Matter.MTRClusterAccountLogin
  ( MTRClusterAccountLogin
  , IsMTRClusterAccountLogin(..)
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completion
  , loginWithParams_expectedValues_expectedValueInterval_completion
  , logoutWithParams_expectedValues_expectedValueInterval_completion
  , logoutWithExpectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler
  , loginWithParams_expectedValues_expectedValueInterval_completionHandler
  , logoutWithParams_expectedValues_expectedValueInterval_completionHandler
  , logoutWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector
  , loginWithParams_expectedValues_expectedValueInterval_completionSelector
  , logoutWithParams_expectedValues_expectedValueInterval_completionSelector
  , logoutWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- getSetupPINWithParams:expectedValues:expectedValueInterval:completion:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterGetSetupPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- loginWithParams:expectedValues:expectedValueInterval:completion:@
loginWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLoginParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
loginWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "loginWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- logoutWithParams:expectedValues:expectedValueInterval:completion:@
logoutWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLogoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- logoutWithExpectedValues:expectedValueInterval:completion:@
logoutWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithExpectedValues_expectedValueInterval_completion mtrClusterAccountLogin  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAccountLogin (mkSelector "logoutWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAccountLogin  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccountLogin (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAccountLogin  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccountLogin (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAccountLogin  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccountLogin (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAccountLogin  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccountLogin (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAccountLogin  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccountLogin (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAccountLogin mtrClusterAccountLogin => mtrClusterAccountLogin -> IO (Id MTRClusterAccountLogin)
init_ mtrClusterAccountLogin  =
    sendMsg mtrClusterAccountLogin (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAccountLogin)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAccountLogin"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRDevice device, IsNSObject queue) => mtrClusterAccountLogin -> device -> CUShort -> queue -> IO (Id MTRClusterAccountLogin)
initWithDevice_endpoint_queue mtrClusterAccountLogin  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterAccountLogin (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterGetSetupPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- loginWithParams:expectedValues:expectedValueInterval:completionHandler:@
loginWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLoginParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
loginWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "loginWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- logoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
logoutWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLogoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccountLogin (mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- logoutWithExpectedValues:expectedValueInterval:completionHandler:@
logoutWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithExpectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAccountLogin (mkSelector "logoutWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAccountLogin -> device -> endpointID -> queue -> IO (Id MTRClusterAccountLogin)
initWithDevice_endpointID_queue mtrClusterAccountLogin  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAccountLogin (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSetupPINWithParams:expectedValues:expectedValueInterval:completion:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @loginWithParams:expectedValues:expectedValueInterval:completion:@
loginWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
loginWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "loginWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @logoutWithParams:expectedValues:expectedValueInterval:completion:@
logoutWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
logoutWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @logoutWithExpectedValues:expectedValueInterval:completion:@
logoutWithExpectedValues_expectedValueInterval_completionSelector :: Selector
logoutWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "logoutWithExpectedValues:expectedValueInterval:completion:"

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

-- | @Selector@ for @getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @loginWithParams:expectedValues:expectedValueInterval:completionHandler:@
loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "loginWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @logoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @logoutWithExpectedValues:expectedValueInterval:completionHandler:@
logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "logoutWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

