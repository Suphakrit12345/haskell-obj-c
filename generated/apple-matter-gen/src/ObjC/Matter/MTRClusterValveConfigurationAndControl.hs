{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Valve Configuration and Control    This cluster is used to configure a valve.
--
-- Generated bindings for @MTRClusterValveConfigurationAndControl@.
module ObjC.Matter.MTRClusterValveConfigurationAndControl
  ( MTRClusterValveConfigurationAndControl
  , IsMTRClusterValveConfigurationAndControl(..)
  , openWithParams_expectedValues_expectedValueInterval_completion
  , openWithExpectedValues_expectedValueInterval_completion
  , closeWithParams_expectedValues_expectedValueInterval_completion
  , closeWithExpectedValues_expectedValueInterval_completion
  , readAttributeOpenDurationWithParams
  , readAttributeDefaultOpenDurationWithParams
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params
  , readAttributeAutoCloseTimeWithParams
  , readAttributeRemainingDurationWithParams
  , readAttributeCurrentStateWithParams
  , readAttributeTargetStateWithParams
  , readAttributeCurrentLevelWithParams
  , readAttributeTargetLevelWithParams
  , readAttributeDefaultOpenLevelWithParams
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params
  , readAttributeValveFaultWithParams
  , readAttributeLevelStepWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , openWithParams_expectedValues_expectedValueInterval_completionSelector
  , openWithExpectedValues_expectedValueInterval_completionSelector
  , closeWithParams_expectedValues_expectedValueInterval_completionSelector
  , closeWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeOpenDurationWithParamsSelector
  , readAttributeDefaultOpenDurationWithParamsSelector
  , writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector
  , readAttributeAutoCloseTimeWithParamsSelector
  , readAttributeRemainingDurationWithParamsSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeTargetStateWithParamsSelector
  , readAttributeCurrentLevelWithParamsSelector
  , readAttributeTargetLevelWithParamsSelector
  , readAttributeDefaultOpenLevelWithParamsSelector
  , writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeValveFaultWithParamsSelector
  , readAttributeLevelStepWithParamsSelector
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

-- | @- openWithParams:expectedValues:expectedValueInterval:completion:@
openWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openWithParams_expectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterValveConfigurationAndControl (mkSelector "openWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openWithExpectedValues:expectedValueInterval:completion:@
openWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
openWithExpectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterValveConfigurationAndControl (mkSelector "openWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- closeWithParams:expectedValues:expectedValueInterval:completion:@
closeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
closeWithParams_expectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterValveConfigurationAndControl (mkSelector "closeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- closeWithExpectedValues:expectedValueInterval:completion:@
closeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
closeWithExpectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterValveConfigurationAndControl (mkSelector "closeWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOpenDurationWithParams:@
readAttributeOpenDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeOpenDurationWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeOpenDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultOpenDurationWithParams:@
readAttributeDefaultOpenDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeDefaultOpenDurationWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeDefaultOpenDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval mtrClusterValveConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params mtrClusterValveConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAutoCloseTimeWithParams:@
readAttributeAutoCloseTimeWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAutoCloseTimeWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeAutoCloseTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRemainingDurationWithParams:@
readAttributeRemainingDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeRemainingDurationWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeRemainingDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeCurrentStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeTargetStateWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeTargetStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCurrentLevelWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeCurrentLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetLevelWithParams:@
readAttributeTargetLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeTargetLevelWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeTargetLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultOpenLevelWithParams:@
readAttributeDefaultOpenLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeDefaultOpenLevelWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeDefaultOpenLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval mtrClusterValveConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params mtrClusterValveConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeValveFaultWithParams:@
readAttributeValveFaultWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeValveFaultWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeValveFaultWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLevelStepWithParams:@
readAttributeLevelStepWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLevelStepWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeLevelStepWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterValveConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterValveConfigurationAndControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl => mtrClusterValveConfigurationAndControl -> IO (Id MTRClusterValveConfigurationAndControl)
init_ mtrClusterValveConfigurationAndControl  =
    sendMsg mtrClusterValveConfigurationAndControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterValveConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterValveConfigurationAndControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterValveConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRClusterValveConfigurationAndControl)
initWithDevice_endpointID_queue mtrClusterValveConfigurationAndControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterValveConfigurationAndControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithParams:expectedValues:expectedValueInterval:completion:@
openWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
openWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @openWithExpectedValues:expectedValueInterval:completion:@
openWithExpectedValues_expectedValueInterval_completionSelector :: Selector
openWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "openWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @closeWithParams:expectedValues:expectedValueInterval:completion:@
closeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
closeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "closeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @closeWithExpectedValues:expectedValueInterval:completion:@
closeWithExpectedValues_expectedValueInterval_completionSelector :: Selector
closeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "closeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOpenDurationWithParams:@
readAttributeOpenDurationWithParamsSelector :: Selector
readAttributeOpenDurationWithParamsSelector = mkSelector "readAttributeOpenDurationWithParams:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithParams:@
readAttributeDefaultOpenDurationWithParamsSelector :: Selector
readAttributeDefaultOpenDurationWithParamsSelector = mkSelector "readAttributeDefaultOpenDurationWithParams:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:@
writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithParams:@
readAttributeAutoCloseTimeWithParamsSelector :: Selector
readAttributeAutoCloseTimeWithParamsSelector = mkSelector "readAttributeAutoCloseTimeWithParams:"

-- | @Selector@ for @readAttributeRemainingDurationWithParams:@
readAttributeRemainingDurationWithParamsSelector :: Selector
readAttributeRemainingDurationWithParamsSelector = mkSelector "readAttributeRemainingDurationWithParams:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParamsSelector :: Selector
readAttributeTargetStateWithParamsSelector = mkSelector "readAttributeTargetStateWithParams:"

-- | @Selector@ for @readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParamsSelector :: Selector
readAttributeCurrentLevelWithParamsSelector = mkSelector "readAttributeCurrentLevelWithParams:"

-- | @Selector@ for @readAttributeTargetLevelWithParams:@
readAttributeTargetLevelWithParamsSelector :: Selector
readAttributeTargetLevelWithParamsSelector = mkSelector "readAttributeTargetLevelWithParams:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithParams:@
readAttributeDefaultOpenLevelWithParamsSelector :: Selector
readAttributeDefaultOpenLevelWithParamsSelector = mkSelector "readAttributeDefaultOpenLevelWithParams:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:@
writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeValveFaultWithParams:@
readAttributeValveFaultWithParamsSelector :: Selector
readAttributeValveFaultWithParamsSelector = mkSelector "readAttributeValveFaultWithParams:"

-- | @Selector@ for @readAttributeLevelStepWithParams:@
readAttributeLevelStepWithParamsSelector :: Selector
readAttributeLevelStepWithParamsSelector = mkSelector "readAttributeLevelStepWithParams:"

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

