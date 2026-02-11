{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Input    This cluster provides an interface for controlling the Input Selector on a media device such as a TV.
--
-- Generated bindings for @MTRClusterMediaInput@.
module ObjC.Matter.MTRClusterMediaInput
  ( MTRClusterMediaInput
  , IsMTRClusterMediaInput(..)
  , selectInputWithParams_expectedValues_expectedValueInterval_completion
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completion
  , showInputStatusWithExpectedValues_expectedValueInterval_completion
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completion
  , hideInputStatusWithExpectedValues_expectedValueInterval_completion
  , renameInputWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeInputListWithParams
  , readAttributeCurrentInputWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , selectInputWithParams_expectedValues_expectedValueInterval_completionHandler
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler
  , showInputStatusWithExpectedValues_expectedValueInterval_completionHandler
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler
  , renameInputWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , selectInputWithParams_expectedValues_expectedValueInterval_completionSelector
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , showInputStatusWithExpectedValues_expectedValueInterval_completionSelector
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector
  , renameInputWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeInputListWithParamsSelector
  , readAttributeCurrentInputWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- selectInputWithParams:expectedValues:expectedValueInterval:completion:@
selectInputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectInputWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- showInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- showInputStatusWithExpectedValues:expectedValueInterval:completion:@
showInputStatusWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithExpectedValues_expectedValueInterval_completion mtrClusterMediaInput  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaInput (mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- hideInputStatusWithExpectedValues:expectedValueInterval:completion:@
hideInputStatusWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithExpectedValues_expectedValueInterval_completion mtrClusterMediaInput  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaInput (mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- renameInputWithParams:expectedValues:expectedValueInterval:completion:@
renameInputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameInputWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInputListWithParams:@
readAttributeInputListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeInputListWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeInputListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentInputWithParams:@
readAttributeCurrentInputWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeCurrentInputWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeCurrentInputWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMediaInput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaInput (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMediaInput mtrClusterMediaInput => mtrClusterMediaInput -> IO (Id MTRClusterMediaInput)
init_ mtrClusterMediaInput  =
    sendMsg mtrClusterMediaInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMediaInput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMediaInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRDevice device, IsNSObject queue) => mtrClusterMediaInput -> device -> CUShort -> queue -> IO (Id MTRClusterMediaInput)
initWithDevice_endpoint_queue mtrClusterMediaInput  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterMediaInput (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectInputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectInputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaInput (mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaInput (mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameInputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameInputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaInput (mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMediaInput -> device -> endpointID -> queue -> IO (Id MTRClusterMediaInput)
initWithDevice_endpointID_queue mtrClusterMediaInput  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMediaInput (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectInputWithParams:expectedValues:expectedValueInterval:completion:@
selectInputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
selectInputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @showInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @showInputStatusWithExpectedValues:expectedValueInterval:completion:@
showInputStatusWithExpectedValues_expectedValueInterval_completionSelector :: Selector
showInputStatusWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideInputStatusWithExpectedValues:expectedValueInterval:completion:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector :: Selector
hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @renameInputWithParams:expectedValues:expectedValueInterval:completion:@
renameInputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
renameInputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeInputListWithParams:@
readAttributeInputListWithParamsSelector :: Selector
readAttributeInputListWithParamsSelector = mkSelector "readAttributeInputListWithParams:"

-- | @Selector@ for @readAttributeCurrentInputWithParams:@
readAttributeCurrentInputWithParamsSelector :: Selector
readAttributeCurrentInputWithParamsSelector = mkSelector "readAttributeCurrentInputWithParams:"

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

-- | @Selector@ for @selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

