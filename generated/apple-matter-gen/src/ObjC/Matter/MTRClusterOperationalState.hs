{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of any device where a state machine is a part of the operation.
--
-- Generated bindings for @MTRClusterOperationalState@.
module ObjC.Matter.MTRClusterOperationalState
  ( MTRClusterOperationalState
  , IsMTRClusterOperationalState(..)
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startWithParams_expectedValues_expectedValueInterval_completion
  , startWithExpectedValues_expectedValueInterval_completion
  , resumeWithParams_expectedValues_expectedValueInterval_completion
  , resumeWithExpectedValues_expectedValueInterval_completion
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
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithExpectedValues_expectedValueInterval_completionSelector
  , startWithParams_expectedValues_expectedValueInterval_completionSelector
  , startWithExpectedValues_expectedValueInterval_completionSelector
  , resumeWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeWithExpectedValues_expectedValueInterval_completionSelector
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
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalState (mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOperationalState (mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalState (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOperationalState (mkSelector "stopWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterStartParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalState (mkSelector "startWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOperationalState (mkSelector "startWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterResumeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalState (mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOperationalState (mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributePhaseListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeCurrentPhaseWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeCountdownTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeOperationalStateListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeOperationalStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeOperationalErrorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalState (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOperationalState mtrClusterOperationalState => mtrClusterOperationalState -> IO (Id MTRClusterOperationalState)
init_ mtrClusterOperationalState  =
    sendMsg mtrClusterOperationalState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOperationalState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterOperationalState)
initWithDevice_endpointID_queue mtrClusterOperationalState  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOperationalState (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completionSelector :: Selector
startWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resumeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resumeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:"

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

