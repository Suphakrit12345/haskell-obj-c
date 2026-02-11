{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Keypad Input    This cluster provides an interface for controlling a device like a TV using action commands such as UP, DOWN, and SELECT.
--
-- Generated bindings for @MTRClusterKeypadInput@.
module ObjC.Matter.MTRClusterKeypadInput
  ( MTRClusterKeypadInput
  , IsMTRClusterKeypadInput(..)
  , sendKeyWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- sendKeyWithParams:expectedValues:expectedValueInterval:completion:@
sendKeyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRKeypadInputClusterSendKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterKeypadInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sendKeyWithParams_expectedValues_expectedValueInterval_completion mtrClusterKeypadInput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterKeypadInput (mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterKeypadInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterKeypadInput (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterKeypadInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterKeypadInput (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterKeypadInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterKeypadInput (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterKeypadInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterKeypadInput (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterKeypadInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterKeypadInput (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterKeypadInput mtrClusterKeypadInput => mtrClusterKeypadInput -> IO (Id MTRClusterKeypadInput)
init_ mtrClusterKeypadInput  =
    sendMsg mtrClusterKeypadInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterKeypadInput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterKeypadInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRDevice device, IsNSObject queue) => mtrClusterKeypadInput -> device -> CUShort -> queue -> IO (Id MTRClusterKeypadInput)
initWithDevice_endpoint_queue mtrClusterKeypadInput  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterKeypadInput (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRKeypadInputClusterSendKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterKeypadInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterKeypadInput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterKeypadInput (mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterKeypadInput -> device -> endpointID -> queue -> IO (Id MTRClusterKeypadInput)
initWithDevice_endpointID_queue mtrClusterKeypadInput  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterKeypadInput (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendKeyWithParams:expectedValues:expectedValueInterval:completion:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completion:"

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

-- | @Selector@ for @sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

