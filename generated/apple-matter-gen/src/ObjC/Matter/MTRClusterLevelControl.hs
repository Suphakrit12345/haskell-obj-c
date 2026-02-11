{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Level Control    Attributes and commands for controlling devices that can be set to a level between fully 'On' and fully 'Off.'
--
-- Generated bindings for @MTRClusterLevelControl@.
module ObjC.Matter.MTRClusterLevelControl
  ( MTRClusterLevelControl
  , IsMTRClusterLevelControl(..)
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completion
  , moveWithParams_expectedValues_expectedValueInterval_completion
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentLevelWithParams
  , readAttributeRemainingTimeWithParams
  , readAttributeMinLevelWithParams
  , readAttributeMaxLevelWithParams
  , readAttributeCurrentFrequencyWithParams
  , readAttributeMinFrequencyWithParams
  , readAttributeMaxFrequencyWithParams
  , readAttributeOptionsWithParams
  , writeAttributeOptionsWithValue_expectedValueInterval
  , writeAttributeOptionsWithValue_expectedValueInterval_params
  , readAttributeOnOffTransitionTimeWithParams
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeOnLevelWithParams
  , writeAttributeOnLevelWithValue_expectedValueInterval
  , writeAttributeOnLevelWithValue_expectedValueInterval_params
  , readAttributeOnTransitionTimeWithParams
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeOffTransitionTimeWithParams
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeDefaultMoveRateWithParams
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params
  , readAttributeStartUpCurrentLevelWithParams
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentLevelWithParamsSelector
  , readAttributeRemainingTimeWithParamsSelector
  , readAttributeMinLevelWithParamsSelector
  , readAttributeMaxLevelWithParamsSelector
  , readAttributeCurrentFrequencyWithParamsSelector
  , readAttributeMinFrequencyWithParamsSelector
  , readAttributeMaxFrequencyWithParamsSelector
  , readAttributeOptionsWithParamsSelector
  , writeAttributeOptionsWithValue_expectedValueIntervalSelector
  , writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector
  , readAttributeOnOffTransitionTimeWithParamsSelector
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeOnLevelWithParamsSelector
  , writeAttributeOnLevelWithValue_expectedValueIntervalSelector
  , writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeOnTransitionTimeWithParamsSelector
  , writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeOffTransitionTimeWithParamsSelector
  , writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeDefaultMoveRateWithParamsSelector
  , writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector
  , readAttributeStartUpCurrentLevelWithParamsSelector
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- moveToLevelWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveWithParams:expectedValues:expectedValueInterval:completion:@
moveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeCurrentLevelWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeCurrentLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeRemainingTimeWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeRemainingTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMinLevelWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeMinLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMaxLevelWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeMaxLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentFrequencyWithParams:@
readAttributeCurrentFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeCurrentFrequencyWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeCurrentFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinFrequencyWithParams:@
readAttributeMinFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMinFrequencyWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeMinFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxFrequencyWithParams:@
readAttributeMaxFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMaxFrequencyWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeMaxFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOptionsWithParams:@
readAttributeOptionsWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOptionsWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeOptionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOnOffTransitionTimeWithParams:@
readAttributeOnOffTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnOffTransitionTimeWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeOnOffTransitionTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOnLevelWithParams:@
readAttributeOnLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnLevelWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeOnLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOnLevelWithValue:expectedValueInterval:@
writeAttributeOnLevelWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnLevelWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOnLevelWithValue:expectedValueInterval:params:@
writeAttributeOnLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnLevelWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOnTransitionTimeWithParams:@
readAttributeOnTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnTransitionTimeWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeOnTransitionTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOnTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOffTransitionTimeWithParams:@
readAttributeOffTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOffTransitionTimeWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeOffTransitionTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOffTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeDefaultMoveRateWithParams:@
readAttributeDefaultMoveRateWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeDefaultMoveRateWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeDefaultMoveRateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDefaultMoveRateWithValue:expectedValueInterval:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultMoveRateWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStartUpCurrentLevelWithParams:@
readAttributeStartUpCurrentLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeStartUpCurrentLevelWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeStartUpCurrentLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params mtrClusterLevelControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLevelControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLevelControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterLevelControl mtrClusterLevelControl => mtrClusterLevelControl -> IO (Id MTRClusterLevelControl)
init_ mtrClusterLevelControl  =
    sendMsg mtrClusterLevelControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterLevelControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLevelControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRDevice device, IsNSObject queue) => mtrClusterLevelControl -> device -> CUShort -> queue -> IO (Id MTRClusterLevelControl)
initWithDevice_endpoint_queue mtrClusterLevelControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterLevelControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stepWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLevelControl (mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLevelControl -> device -> endpointID -> queue -> IO (Id MTRClusterLevelControl)
initWithDevice_endpointID_queue mtrClusterLevelControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterLevelControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveToLevelWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveWithParams:expectedValues:expectedValueInterval:completion:@
moveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParamsSelector :: Selector
readAttributeCurrentLevelWithParamsSelector = mkSelector "readAttributeCurrentLevelWithParams:"

-- | @Selector@ for @readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParamsSelector :: Selector
readAttributeRemainingTimeWithParamsSelector = mkSelector "readAttributeRemainingTimeWithParams:"

-- | @Selector@ for @readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParamsSelector :: Selector
readAttributeMinLevelWithParamsSelector = mkSelector "readAttributeMinLevelWithParams:"

-- | @Selector@ for @readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParamsSelector :: Selector
readAttributeMaxLevelWithParamsSelector = mkSelector "readAttributeMaxLevelWithParams:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithParams:@
readAttributeCurrentFrequencyWithParamsSelector :: Selector
readAttributeCurrentFrequencyWithParamsSelector = mkSelector "readAttributeCurrentFrequencyWithParams:"

-- | @Selector@ for @readAttributeMinFrequencyWithParams:@
readAttributeMinFrequencyWithParamsSelector :: Selector
readAttributeMinFrequencyWithParamsSelector = mkSelector "readAttributeMinFrequencyWithParams:"

-- | @Selector@ for @readAttributeMaxFrequencyWithParams:@
readAttributeMaxFrequencyWithParamsSelector :: Selector
readAttributeMaxFrequencyWithParamsSelector = mkSelector "readAttributeMaxFrequencyWithParams:"

-- | @Selector@ for @readAttributeOptionsWithParams:@
readAttributeOptionsWithParamsSelector :: Selector
readAttributeOptionsWithParamsSelector = mkSelector "readAttributeOptionsWithParams:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOptionsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithParams:@
readAttributeOnOffTransitionTimeWithParamsSelector :: Selector
readAttributeOnOffTransitionTimeWithParamsSelector = mkSelector "readAttributeOnOffTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnLevelWithParams:@
readAttributeOnLevelWithParamsSelector :: Selector
readAttributeOnLevelWithParamsSelector = mkSelector "readAttributeOnLevelWithParams:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:expectedValueInterval:@
writeAttributeOnLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOnLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:expectedValueInterval:params:@
writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithParams:@
readAttributeOnTransitionTimeWithParamsSelector :: Selector
readAttributeOnTransitionTimeWithParamsSelector = mkSelector "readAttributeOnTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithParams:@
readAttributeOffTransitionTimeWithParamsSelector :: Selector
readAttributeOffTransitionTimeWithParamsSelector = mkSelector "readAttributeOffTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithParams:@
readAttributeDefaultMoveRateWithParamsSelector :: Selector
readAttributeDefaultMoveRateWithParamsSelector = mkSelector "readAttributeDefaultMoveRateWithParams:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:expectedValueInterval:@
writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithParams:@
readAttributeStartUpCurrentLevelWithParamsSelector :: Selector
readAttributeStartUpCurrentLevelWithParamsSelector = mkSelector "readAttributeStartUpCurrentLevelWithParams:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

