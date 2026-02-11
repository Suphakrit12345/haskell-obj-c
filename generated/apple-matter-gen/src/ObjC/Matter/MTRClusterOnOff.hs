{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/Off    Attributes and commands for switching devices between 'On' and 'Off' states.
--
-- Generated bindings for @MTRClusterOnOff@.
module ObjC.Matter.MTRClusterOnOff
  ( MTRClusterOnOff
  , IsMTRClusterOnOff(..)
  , offWithParams_expectedValues_expectedValueInterval_completion
  , offWithExpectedValues_expectedValueInterval_completion
  , onWithParams_expectedValues_expectedValueInterval_completion
  , onWithExpectedValues_expectedValueInterval_completion
  , toggleWithParams_expectedValues_expectedValueInterval_completion
  , toggleWithExpectedValues_expectedValueInterval_completion
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completion
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeOnOffWithParams
  , readAttributeGlobalSceneControlWithParams
  , readAttributeOnTimeWithParams
  , writeAttributeOnTimeWithValue_expectedValueInterval
  , writeAttributeOnTimeWithValue_expectedValueInterval_params
  , readAttributeOffWaitTimeWithParams
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval_params
  , readAttributeStartUpOnOffWithParams
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , offWithParams_expectedValues_expectedValueInterval_completionHandler
  , offWithExpectedValues_expectedValueInterval_completionHandler
  , onWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithExpectedValues_expectedValueInterval_completionHandler
  , toggleWithParams_expectedValues_expectedValueInterval_completionHandler
  , toggleWithExpectedValues_expectedValueInterval_completionHandler
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , offWithParams_expectedValues_expectedValueInterval_completionSelector
  , offWithExpectedValues_expectedValueInterval_completionSelector
  , onWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithExpectedValues_expectedValueInterval_completionSelector
  , toggleWithParams_expectedValues_expectedValueInterval_completionSelector
  , toggleWithExpectedValues_expectedValueInterval_completionSelector
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeOnOffWithParamsSelector
  , readAttributeGlobalSceneControlWithParamsSelector
  , readAttributeOnTimeWithParamsSelector
  , writeAttributeOnTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeOffWaitTimeWithParamsSelector
  , writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeStartUpOnOffWithParamsSelector
  , writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , offWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- offWithParams:expectedValues:expectedValueInterval:completion:@
offWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "offWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- offWithExpectedValues:expectedValueInterval:completion:@
offWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "offWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithParams:expectedValues:expectedValueInterval:completion:@
onWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithExpectedValues:expectedValueInterval:completion:@
onWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "onWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- toggleWithParams:expectedValues:expectedValueInterval:completion:@
toggleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterToggleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- toggleWithExpectedValues:expectedValueInterval:completion:@
toggleWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "toggleWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- offWithEffectWithParams:expectedValues:expectedValueInterval:completion:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffWithEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnOffWithParams:@
readAttributeOnOffWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOnOffWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeOnOffWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGlobalSceneControlWithParams:@
readAttributeGlobalSceneControlWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeGlobalSceneControlWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeGlobalSceneControlWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOnTimeWithParams:@
readAttributeOnTimeWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOnTimeWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeOnTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOnTimeWithValue:expectedValueInterval:@
writeAttributeOnTimeWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnTimeWithValue_expectedValueInterval mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOnTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTimeWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnTimeWithValue_expectedValueInterval_params mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOffWaitTimeWithParams:@
readAttributeOffWaitTimeWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOffWaitTimeWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeOffWaitTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOffWaitTimeWithValue:expectedValueInterval:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOffWaitTimeWithValue_expectedValueInterval mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOffWaitTimeWithValue_expectedValueInterval_params mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStartUpOnOffWithParams:@
readAttributeStartUpOnOffWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeStartUpOnOffWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeStartUpOnOffWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStartUpOnOffWithValue:expectedValueInterval:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpOnOffWithValue_expectedValueInterval mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpOnOffWithValue_expectedValueInterval_params mtrClusterOnOff  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOnOff  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOff (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOnOff mtrClusterOnOff => mtrClusterOnOff -> IO (Id MTRClusterOnOff)
init_ mtrClusterOnOff  =
    sendMsg mtrClusterOnOff (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOnOff)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOnOff"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRDevice device, IsNSObject queue) => mtrClusterOnOff -> device -> CUShort -> queue -> IO (Id MTRClusterOnOff)
initWithDevice_endpoint_queue mtrClusterOnOff  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOnOff (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- offWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "offWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- offWithExpectedValues:expectedValueInterval:completionHandler:@
offWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "offWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithExpectedValues:expectedValueInterval:completionHandler:@
onWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "onWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- toggleWithParams:expectedValues:expectedValueInterval:completionHandler:@
toggleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterToggleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- toggleWithExpectedValues:expectedValueInterval:completionHandler:@
toggleWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "toggleWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffWithEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOff (mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOnOff (mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOnOff -> device -> endpointID -> queue -> IO (Id MTRClusterOnOff)
initWithDevice_endpointID_queue mtrClusterOnOff  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOnOff (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offWithParams:expectedValues:expectedValueInterval:completion:@
offWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
offWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @offWithExpectedValues:expectedValueInterval:completion:@
offWithExpectedValues_expectedValueInterval_completionSelector :: Selector
offWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "offWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithParams:expectedValues:expectedValueInterval:completion:@
onWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
onWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithExpectedValues:expectedValueInterval:completion:@
onWithExpectedValues_expectedValueInterval_completionSelector :: Selector
onWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "onWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @toggleWithParams:expectedValues:expectedValueInterval:completion:@
toggleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
toggleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @toggleWithExpectedValues:expectedValueInterval:completion:@
toggleWithExpectedValues_expectedValueInterval_completionSelector :: Selector
toggleWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "toggleWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @offWithEffectWithParams:expectedValues:expectedValueInterval:completion:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector :: Selector
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOnOffWithParams:@
readAttributeOnOffWithParamsSelector :: Selector
readAttributeOnOffWithParamsSelector = mkSelector "readAttributeOnOffWithParams:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithParams:@
readAttributeGlobalSceneControlWithParamsSelector :: Selector
readAttributeGlobalSceneControlWithParamsSelector = mkSelector "readAttributeGlobalSceneControlWithParams:"

-- | @Selector@ for @readAttributeOnTimeWithParams:@
readAttributeOnTimeWithParamsSelector :: Selector
readAttributeOnTimeWithParamsSelector = mkSelector "readAttributeOnTimeWithParams:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:expectedValueInterval:@
writeAttributeOnTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOnTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOffWaitTimeWithParams:@
readAttributeOffWaitTimeWithParamsSelector :: Selector
readAttributeOffWaitTimeWithParamsSelector = mkSelector "readAttributeOffWaitTimeWithParams:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:expectedValueInterval:@
writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStartUpOnOffWithParams:@
readAttributeStartUpOnOffWithParamsSelector :: Selector
readAttributeStartUpOnOffWithParamsSelector = mkSelector "readAttributeStartUpOnOffWithParams:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:expectedValueInterval:@
writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @offWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @offWithExpectedValues:expectedValueInterval:completionHandler:@
offWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
offWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithExpectedValues:expectedValueInterval:completionHandler:@
onWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
onWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @toggleWithParams:expectedValues:expectedValueInterval:completionHandler:@
toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @toggleWithExpectedValues:expectedValueInterval:completionHandler:@
toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "toggleWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

