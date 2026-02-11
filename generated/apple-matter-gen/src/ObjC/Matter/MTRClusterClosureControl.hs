{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Control    This cluster provides an interface for controlling a Closure.
--
-- Generated bindings for @MTRClusterClosureControl@.
module ObjC.Matter.MTRClusterClosureControl
  ( MTRClusterClosureControl
  , IsMTRClusterClosureControl(..)
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , moveToWithParams_expectedValues_expectedValueInterval_completion
  , moveToWithExpectedValues_expectedValueInterval_completion
  , calibrateWithParams_expectedValues_expectedValueInterval_completion
  , calibrateWithExpectedValues_expectedValueInterval_completion
  , readAttributeCountdownTimeWithParams
  , readAttributeMainStateWithParams
  , readAttributeCurrentErrorListWithParams
  , readAttributeOverallCurrentStateWithParams
  , readAttributeOverallTargetStateWithParams
  , readAttributeLatchControlModesWithParams
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
  , moveToWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToWithExpectedValues_expectedValueInterval_completionSelector
  , calibrateWithParams_expectedValues_expectedValueInterval_completionSelector
  , calibrateWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeCountdownTimeWithParamsSelector
  , readAttributeMainStateWithParamsSelector
  , readAttributeCurrentErrorListWithParamsSelector
  , readAttributeOverallCurrentStateWithParamsSelector
  , readAttributeOverallTargetStateWithParamsSelector
  , readAttributeLatchControlModesWithParamsSelector
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
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterClosureControl (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterClosureControl (mkSelector "stopWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToWithParams:expectedValues:expectedValueInterval:completion:@
moveToWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterMoveToParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterClosureControl (mkSelector "moveToWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToWithExpectedValues:expectedValueInterval:completion:@
moveToWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterClosureControl (mkSelector "moveToWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- calibrateWithParams:expectedValues:expectedValueInterval:completion:@
calibrateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterCalibrateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
calibrateWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterClosureControl (mkSelector "calibrateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- calibrateWithExpectedValues:expectedValueInterval:completion:@
calibrateWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
calibrateWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterClosureControl (mkSelector "calibrateWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeCountdownTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMainStateWithParams:@
readAttributeMainStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeMainStateWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeMainStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentErrorListWithParams:@
readAttributeCurrentErrorListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeCurrentErrorListWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeCurrentErrorListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverallCurrentStateWithParams:@
readAttributeOverallCurrentStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeOverallCurrentStateWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeOverallCurrentStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverallTargetStateWithParams:@
readAttributeOverallTargetStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeOverallTargetStateWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeOverallTargetStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeLatchControlModesWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeLatchControlModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterClosureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterClosureControl mtrClusterClosureControl => mtrClusterClosureControl -> IO (Id MTRClusterClosureControl)
init_ mtrClusterClosureControl  =
    sendMsg mtrClusterClosureControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterClosureControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterClosureControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterClosureControl -> device -> endpointID -> queue -> IO (Id MTRClusterClosureControl)
initWithDevice_endpointID_queue mtrClusterClosureControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterClosureControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToWithParams:expectedValues:expectedValueInterval:completion:@
moveToWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToWithExpectedValues:expectedValueInterval:completion:@
moveToWithExpectedValues_expectedValueInterval_completionSelector :: Selector
moveToWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "moveToWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @calibrateWithParams:expectedValues:expectedValueInterval:completion:@
calibrateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
calibrateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "calibrateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @calibrateWithExpectedValues:expectedValueInterval:completion:@
calibrateWithExpectedValues_expectedValueInterval_completionSelector :: Selector
calibrateWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "calibrateWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParamsSelector :: Selector
readAttributeCountdownTimeWithParamsSelector = mkSelector "readAttributeCountdownTimeWithParams:"

-- | @Selector@ for @readAttributeMainStateWithParams:@
readAttributeMainStateWithParamsSelector :: Selector
readAttributeMainStateWithParamsSelector = mkSelector "readAttributeMainStateWithParams:"

-- | @Selector@ for @readAttributeCurrentErrorListWithParams:@
readAttributeCurrentErrorListWithParamsSelector :: Selector
readAttributeCurrentErrorListWithParamsSelector = mkSelector "readAttributeCurrentErrorListWithParams:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithParams:@
readAttributeOverallCurrentStateWithParamsSelector :: Selector
readAttributeOverallCurrentStateWithParamsSelector = mkSelector "readAttributeOverallCurrentStateWithParams:"

-- | @Selector@ for @readAttributeOverallTargetStateWithParams:@
readAttributeOverallTargetStateWithParamsSelector :: Selector
readAttributeOverallTargetStateWithParamsSelector = mkSelector "readAttributeOverallTargetStateWithParams:"

-- | @Selector@ for @readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParamsSelector :: Selector
readAttributeLatchControlModesWithParamsSelector = mkSelector "readAttributeLatchControlModesWithParams:"

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

