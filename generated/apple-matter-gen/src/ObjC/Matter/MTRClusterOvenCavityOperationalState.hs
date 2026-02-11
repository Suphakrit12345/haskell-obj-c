{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Oven Cavity Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of an Oven.
--
-- Generated bindings for @MTRClusterOvenCavityOperationalState@.
module ObjC.Matter.MTRClusterOvenCavityOperationalState
  ( MTRClusterOvenCavityOperationalState
  , IsMTRClusterOvenCavityOperationalState(..)
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startWithParams_expectedValues_expectedValueInterval_completion
  , startWithExpectedValues_expectedValueInterval_completion
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
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithExpectedValues_expectedValueInterval_completionSelector
  , startWithParams_expectedValues_expectedValueInterval_completionSelector
  , startWithExpectedValues_expectedValueInterval_completionSelector
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

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOvenCavityOperationalState (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOvenCavityOperationalState (mkSelector "stopWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStartParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithParams_expectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOvenCavityOperationalState (mkSelector "startWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithExpectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOvenCavityOperationalState (mkSelector "startWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributePhaseListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeCurrentPhaseWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeCountdownTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeOperationalStateListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeOperationalStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeOperationalErrorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOvenCavityOperationalState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenCavityOperationalState (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState => mtrClusterOvenCavityOperationalState -> IO (Id MTRClusterOvenCavityOperationalState)
init_ mtrClusterOvenCavityOperationalState  =
    sendMsg mtrClusterOvenCavityOperationalState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOvenCavityOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOvenCavityOperationalState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOvenCavityOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterOvenCavityOperationalState)
initWithDevice_endpointID_queue mtrClusterOvenCavityOperationalState  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOvenCavityOperationalState (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

