{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Requestor    The WebRTC transport requestor cluster provides a way for stream consumers (e.g. Matter Stream Viewer) to establish a WebRTC connection with a stream provider.
--
-- Generated bindings for @MTRClusterWebRTCTransportRequestor@.
module ObjC.Matter.MTRClusterWebRTCTransportRequestor
  ( MTRClusterWebRTCTransportRequestor
  , IsMTRClusterWebRTCTransportRequestor(..)
  , offerWithParams_expectedValues_expectedValueInterval_completion
  , answerWithParams_expectedValues_expectedValueInterval_completion
  , iceCandidatesWithParams_expectedValues_expectedValueInterval_completion
  , endWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSessionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , offerWithParams_expectedValues_expectedValueInterval_completionSelector
  , answerWithParams_expectedValues_expectedValueInterval_completionSelector
  , iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector
  , endWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentSessionsWithParamsSelector
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

-- | @- offerWithParams:expectedValues:expectedValueInterval:completion:@
offerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "offerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- answerWithParams:expectedValues:expectedValueInterval:completion:@
answerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterAnswerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
answerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "answerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
iceCandidatesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterICECandidatesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
iceCandidatesWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- endWithParams:expectedValues:expectedValueInterval:completion:@
endWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterEndParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
endWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "endWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeCurrentSessionsWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeCurrentSessionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWebRTCTransportRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor => mtrClusterWebRTCTransportRequestor -> IO (Id MTRClusterWebRTCTransportRequestor)
init_ mtrClusterWebRTCTransportRequestor  =
    sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWebRTCTransportRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWebRTCTransportRequestor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWebRTCTransportRequestor -> device -> endpointID -> queue -> IO (Id MTRClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queue mtrClusterWebRTCTransportRequestor  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWebRTCTransportRequestor (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerWithParams:expectedValues:expectedValueInterval:completion:@
offerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
offerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @answerWithParams:expectedValues:expectedValueInterval:completion:@
answerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
answerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "answerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @endWithParams:expectedValues:expectedValueInterval:completion:@
endWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
endWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "endWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParamsSelector :: Selector
readAttributeCurrentSessionsWithParamsSelector = mkSelector "readAttributeCurrentSessionsWithParams:"

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

