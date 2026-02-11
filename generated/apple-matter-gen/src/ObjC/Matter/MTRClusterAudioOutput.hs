{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Audio Output    This cluster provides an interface for controlling the Output on a media device such as a TV.
--
-- Generated bindings for @MTRClusterAudioOutput@.
module ObjC.Matter.MTRClusterAudioOutput
  ( MTRClusterAudioOutput
  , IsMTRClusterAudioOutput(..)
  , selectOutputWithParams_expectedValues_expectedValueInterval_completion
  , renameOutputWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeOutputListWithParams
  , readAttributeCurrentOutputWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeOutputListWithParamsSelector
  , readAttributeCurrentOutputWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- selectOutputWithParams:expectedValues:expectedValueInterval:completion:@
selectOutputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterSelectOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectOutputWithParams_expectedValues_expectedValueInterval_completion mtrClusterAudioOutput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAudioOutput (mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- renameOutputWithParams:expectedValues:expectedValueInterval:completion:@
renameOutputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterRenameOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameOutputWithParams_expectedValues_expectedValueInterval_completion mtrClusterAudioOutput  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAudioOutput (mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOutputListWithParams:@
readAttributeOutputListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeOutputListWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeOutputListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentOutputWithParams:@
readAttributeCurrentOutputWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeCurrentOutputWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeCurrentOutputWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAudioOutput  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAudioOutput (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAudioOutput mtrClusterAudioOutput => mtrClusterAudioOutput -> IO (Id MTRClusterAudioOutput)
init_ mtrClusterAudioOutput  =
    sendMsg mtrClusterAudioOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAudioOutput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAudioOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRDevice device, IsNSObject queue) => mtrClusterAudioOutput -> device -> CUShort -> queue -> IO (Id MTRClusterAudioOutput)
initWithDevice_endpoint_queue mtrClusterAudioOutput  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterAudioOutput (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterSelectOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAudioOutput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAudioOutput (mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterRenameOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAudioOutput  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAudioOutput (mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAudioOutput -> device -> endpointID -> queue -> IO (Id MTRClusterAudioOutput)
initWithDevice_endpointID_queue mtrClusterAudioOutput  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAudioOutput (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectOutputWithParams:expectedValues:expectedValueInterval:completion:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @renameOutputWithParams:expectedValues:expectedValueInterval:completion:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOutputListWithParams:@
readAttributeOutputListWithParamsSelector :: Selector
readAttributeOutputListWithParamsSelector = mkSelector "readAttributeOutputListWithParams:"

-- | @Selector@ for @readAttributeCurrentOutputWithParams:@
readAttributeCurrentOutputWithParamsSelector :: Selector
readAttributeCurrentOutputWithParamsSelector = mkSelector "readAttributeCurrentOutputWithParams:"

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

-- | @Selector@ for @selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

