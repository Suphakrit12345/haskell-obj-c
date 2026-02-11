{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Provider    The WebRTC transport provider cluster provides a way for stream providers (e.g. Cameras) to stream or receive their data through WebRTC.
--
-- Generated bindings for @MTRClusterWebRTCTransportProvider@.
module ObjC.Matter.MTRClusterWebRTCTransportProvider
  ( MTRClusterWebRTCTransportProvider
  , IsMTRClusterWebRTCTransportProvider(..)
  , solicitOfferWithParams_expectedValues_expectedValueInterval_completion
  , provideOfferWithParams_expectedValues_expectedValueInterval_completion
  , provideAnswerWithParams_expectedValues_expectedValueInterval_completion
  , provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion
  , endSessionWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSessionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector
  , provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector
  , provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector
  , provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector
  , endSessionWithParams_expectedValues_expectedValueInterval_completionSelector
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

-- | @- solicitOfferWithParams:expectedValues:expectedValueInterval:completion:@
solicitOfferWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterSolicitOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
solicitOfferWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "solicitOfferWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- provideOfferWithParams:expectedValues:expectedValueInterval:completion:@
provideOfferWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideOfferWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "provideOfferWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- provideAnswerWithParams:expectedValues:expectedValueInterval:completion:@
provideAnswerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideAnswerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideAnswerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "provideAnswerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- endSessionWithParams:expectedValues:expectedValueInterval:completion:@
endSessionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterEndSessionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
endSessionWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "endSessionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeCurrentSessionsWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeCurrentSessionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWebRTCTransportProvider  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWebRTCTransportProvider (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider => mtrClusterWebRTCTransportProvider -> IO (Id MTRClusterWebRTCTransportProvider)
init_ mtrClusterWebRTCTransportProvider  =
    sendMsg mtrClusterWebRTCTransportProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWebRTCTransportProvider)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWebRTCTransportProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWebRTCTransportProvider -> device -> endpointID -> queue -> IO (Id MTRClusterWebRTCTransportProvider)
initWithDevice_endpointID_queue mtrClusterWebRTCTransportProvider  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWebRTCTransportProvider (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @solicitOfferWithParams:expectedValues:expectedValueInterval:completion:@
solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "solicitOfferWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideOfferWithParams:expectedValues:expectedValueInterval:completion:@
provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideOfferWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideAnswerWithParams:expectedValues:expectedValueInterval:completion:@
provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideAnswerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @endSessionWithParams:expectedValues:expectedValueInterval:completion:@
endSessionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
endSessionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "endSessionWithParams:expectedValues:expectedValueInterval:completion:"

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

