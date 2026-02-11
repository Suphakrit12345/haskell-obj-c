{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Timer    This cluster supports creating a simple timer functionality.
--
-- Generated bindings for @MTRClusterTimer@.
module ObjC.Matter.MTRClusterTimer
  ( MTRClusterTimer
  , IsMTRClusterTimer(..)
  , setTimerWithParams_expectedValues_expectedValueInterval_completion
  , resetTimerWithParams_expectedValues_expectedValueInterval_completion
  , resetTimerWithExpectedValues_expectedValueInterval_completion
  , addTimeWithParams_expectedValues_expectedValueInterval_completion
  , reduceTimeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSetTimeWithParams
  , readAttributeTimeRemainingWithParams
  , readAttributeTimerStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , setTimerWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetTimerWithExpectedValues_expectedValueInterval_completionSelector
  , addTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSetTimeWithParamsSelector
  , readAttributeTimeRemainingWithParamsSelector
  , readAttributeTimerStateWithParamsSelector
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

-- | @- setTimerWithParams:expectedValues:expectedValueInterval:completion:@
setTimerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterSetTimerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTimerWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimer (mkSelector "setTimerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetTimerWithParams:expectedValues:expectedValueInterval:completion:@
resetTimerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterResetTimerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetTimerWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimer (mkSelector "resetTimerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetTimerWithExpectedValues:expectedValueInterval:completion:@
resetTimerWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetTimerWithExpectedValues_expectedValueInterval_completion mtrClusterTimer  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTimer (mkSelector "resetTimerWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addTimeWithParams:expectedValues:expectedValueInterval:completion:@
addTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterAddTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimer (mkSelector "addTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- reduceTimeWithParams:expectedValues:expectedValueInterval:completion:@
reduceTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterReduceTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reduceTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimer (mkSelector "reduceTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSetTimeWithParams:@
readAttributeSetTimeWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeSetTimeWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeSetTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeRemainingWithParams:@
readAttributeTimeRemainingWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeTimeRemainingWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeTimeRemainingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimerStateWithParams:@
readAttributeTimerStateWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeTimerStateWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeTimerStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimer  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimer (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTimer mtrClusterTimer => mtrClusterTimer -> IO (Id MTRClusterTimer)
init_ mtrClusterTimer  =
    sendMsg mtrClusterTimer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTimer)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimer mtrClusterTimer, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimer -> device -> endpointID -> queue -> IO (Id MTRClusterTimer)
initWithDevice_endpointID_queue mtrClusterTimer  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTimer (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTimerWithParams:expectedValues:expectedValueInterval:completion:@
setTimerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTimerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTimerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetTimerWithParams:expectedValues:expectedValueInterval:completion:@
resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetTimerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetTimerWithExpectedValues:expectedValueInterval:completion:@
resetTimerWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetTimerWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetTimerWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addTimeWithParams:expectedValues:expectedValueInterval:completion:@
addTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @reduceTimeWithParams:expectedValues:expectedValueInterval:completion:@
reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "reduceTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSetTimeWithParams:@
readAttributeSetTimeWithParamsSelector :: Selector
readAttributeSetTimeWithParamsSelector = mkSelector "readAttributeSetTimeWithParams:"

-- | @Selector@ for @readAttributeTimeRemainingWithParams:@
readAttributeTimeRemainingWithParamsSelector :: Selector
readAttributeTimeRemainingWithParamsSelector = mkSelector "readAttributeTimeRemainingWithParams:"

-- | @Selector@ for @readAttributeTimerStateWithParams:@
readAttributeTimerStateWithParamsSelector :: Selector
readAttributeTimerStateWithParamsSelector = mkSelector "readAttributeTimerStateWithParams:"

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

