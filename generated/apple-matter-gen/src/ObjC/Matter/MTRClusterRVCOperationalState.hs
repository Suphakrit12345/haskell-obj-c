{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster RVC Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of a Robotic Vacuum.
--
-- Generated bindings for @MTRClusterRVCOperationalState@.
module ObjC.Matter.MTRClusterRVCOperationalState
  ( MTRClusterRVCOperationalState
  , IsMTRClusterRVCOperationalState(..)
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , resumeWithParams_expectedValues_expectedValueInterval_completion
  , resumeWithExpectedValues_expectedValueInterval_completion
  , goHomeWithParams_expectedValues_expectedValueInterval_completion
  , goHomeWithExpectedValues_expectedValueInterval_completion
  , readAttributePhaseListWithParams
  , readAttributeCurrentPhaseWithParams
  , readAttributeCountdownTimeWithParams
  , readAttributeOperationalStateListWithParams
  , readAttributeOperationalStateWithParams
  , readAttributeOperationalErrorWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , pauseWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseWithExpectedValues_expectedValueInterval_completionSelector
  , resumeWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeWithExpectedValues_expectedValueInterval_completionSelector
  , goHomeWithParams_expectedValues_expectedValueInterval_completionSelector
  , goHomeWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributePhaseListWithParamsSelector
  , readAttributeCurrentPhaseWithParamsSelector
  , readAttributeCountdownTimeWithParamsSelector
  , readAttributeOperationalStateListWithParamsSelector
  , readAttributeOperationalStateWithParamsSelector
  , readAttributeOperationalErrorWithParamsSelector
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

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterRVCOperationalState (mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterRVCOperationalState (mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterResumeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterRVCOperationalState (mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterRVCOperationalState (mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goHomeWithParams:expectedValues:expectedValueInterval:completion:@
goHomeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterGoHomeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goHomeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterRVCOperationalState (mkSelector "goHomeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goHomeWithExpectedValues:expectedValueInterval:completion:@
goHomeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
goHomeWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterRVCOperationalState (mkSelector "goHomeWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributePhaseListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeCurrentPhaseWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeCountdownTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeOperationalStateListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeOperationalStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeOperationalErrorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRVCOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCOperationalState (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState => mtrClusterRVCOperationalState -> IO (Id MTRClusterRVCOperationalState)
init_ mtrClusterRVCOperationalState  =
    sendMsg mtrClusterRVCOperationalState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterRVCOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRVCOperationalState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRVCOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterRVCOperationalState)
initWithDevice_endpointID_queue mtrClusterRVCOperationalState  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterRVCOperationalState (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resumeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resumeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goHomeWithParams:expectedValues:expectedValueInterval:completion:@
goHomeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
goHomeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goHomeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goHomeWithExpectedValues:expectedValueInterval:completion:@
goHomeWithExpectedValues_expectedValueInterval_completionSelector :: Selector
goHomeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "goHomeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePhaseListWithParams:@
readAttributePhaseListWithParamsSelector :: Selector
readAttributePhaseListWithParamsSelector = mkSelector "readAttributePhaseListWithParams:"

-- | @Selector@ for @readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParamsSelector :: Selector
readAttributeCurrentPhaseWithParamsSelector = mkSelector "readAttributeCurrentPhaseWithParams:"

-- | @Selector@ for @readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParamsSelector :: Selector
readAttributeCountdownTimeWithParamsSelector = mkSelector "readAttributeCountdownTimeWithParams:"

-- | @Selector@ for @readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParamsSelector :: Selector
readAttributeOperationalStateListWithParamsSelector = mkSelector "readAttributeOperationalStateListWithParams:"

-- | @Selector@ for @readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParamsSelector :: Selector
readAttributeOperationalStateWithParamsSelector = mkSelector "readAttributeOperationalStateWithParams:"

-- | @Selector@ for @readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParamsSelector :: Selector
readAttributeOperationalErrorWithParamsSelector = mkSelector "readAttributeOperationalErrorWithParams:"

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

