{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content App Observer    This cluster provides an interface for sending targeted commands to an Observer of a Content App on a Video Player device such as a Streaming Media Player, Smart TV or Smart Screen. The cluster server for Content App Observer is implemented by an endpoint that communicates with a Content App, such as a Casting Video Client. The cluster client for Content App Observer is implemented by a Content App endpoint. A Content App is informed of the NodeId of an Observer when a binding is set on the Content App. The Content App can then send the ContentAppMessage to the Observer (server cluster), and the Observer responds with a ContentAppMessageResponse.
--
-- Generated bindings for @MTRClusterContentAppObserver@.
module ObjC.Matter.MTRClusterContentAppObserver
  ( MTRClusterContentAppObserver
  , IsMTRClusterContentAppObserver(..)
  , contentAppMessageWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector
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

-- | @- contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:@
contentAppMessageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRContentAppObserverClusterContentAppMessageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentAppObserver -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
contentAppMessageWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentAppObserver  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentAppObserver (mkSelector "contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentAppObserver  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentAppObserver (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentAppObserver  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentAppObserver (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentAppObserver  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentAppObserver (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentAppObserver  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentAppObserver (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentAppObserver  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentAppObserver (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterContentAppObserver mtrClusterContentAppObserver => mtrClusterContentAppObserver -> IO (Id MTRClusterContentAppObserver)
init_ mtrClusterContentAppObserver  =
    sendMsg mtrClusterContentAppObserver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterContentAppObserver)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentAppObserver"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentAppObserver -> device -> endpointID -> queue -> IO (Id MTRClusterContentAppObserver)
initWithDevice_endpointID_queue mtrClusterContentAppObserver  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterContentAppObserver (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:@
contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:"

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

