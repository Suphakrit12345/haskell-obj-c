{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Barrier Control    This cluster provides control of a barrier (garage door).
--
-- Generated bindings for @MTRClusterBarrierControl@.
module ObjC.Matter.MTRClusterBarrierControl
  ( MTRClusterBarrierControl
  , IsMTRClusterBarrierControl(..)
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completion
  , barrierControlStopWithExpectedValues_expectedValueInterval_completion
  , readAttributeBarrierMovingStateWithParams
  , readAttributeBarrierSafetyStatusWithParams
  , readAttributeBarrierCapabilitiesWithParams
  , readAttributeBarrierOpenEventsWithParams
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCloseEventsWithParams
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCommandOpenEventsWithParams
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCommandCloseEventsWithParams
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierOpenPeriodWithParams
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params
  , readAttributeBarrierClosePeriodWithParams
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params
  , readAttributeBarrierPositionWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeBarrierMovingStateWithParamsSelector
  , readAttributeBarrierSafetyStatusWithParamsSelector
  , readAttributeBarrierCapabilitiesWithParamsSelector
  , readAttributeBarrierOpenEventsWithParamsSelector
  , writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierCloseEventsWithParamsSelector
  , writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierCommandOpenEventsWithParamsSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierCommandCloseEventsWithParamsSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierOpenPeriodWithParamsSelector
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierClosePeriodWithParamsSelector
  , writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeBarrierPositionWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion mtrClusterBarrierControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBarrierControl (mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completion mtrClusterBarrierControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBarrierControl (mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- barrierControlStopWithExpectedValues:expectedValueInterval:completion:@
barrierControlStopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithExpectedValues_expectedValueInterval_completion mtrClusterBarrierControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierMovingStateWithParams:@
readAttributeBarrierMovingStateWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierMovingStateWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierMovingStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBarrierSafetyStatusWithParams:@
readAttributeBarrierSafetyStatusWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierSafetyStatusWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierSafetyStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBarrierCapabilitiesWithParams:@
readAttributeBarrierCapabilitiesWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCapabilitiesWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierCapabilitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBarrierOpenEventsWithParams:@
readAttributeBarrierOpenEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierOpenEventsWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierOpenEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierCloseEventsWithParams:@
readAttributeBarrierCloseEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCloseEventsWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierCloseEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierCommandOpenEventsWithParams:@
readAttributeBarrierCommandOpenEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCommandOpenEventsWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierCommandOpenEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierCommandCloseEventsWithParams:@
readAttributeBarrierCommandCloseEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCommandCloseEventsWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierCommandCloseEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierOpenPeriodWithParams:@
readAttributeBarrierOpenPeriodWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierOpenPeriodWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierOpenPeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierClosePeriodWithParams:@
readAttributeBarrierClosePeriodWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierClosePeriodWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierClosePeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params mtrClusterBarrierControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBarrierPositionWithParams:@
readAttributeBarrierPositionWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierPositionWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeBarrierPositionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBarrierControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBarrierControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBarrierControl mtrClusterBarrierControl => mtrClusterBarrierControl -> IO (Id MTRClusterBarrierControl)
init_ mtrClusterBarrierControl  =
    sendMsg mtrClusterBarrierControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBarrierControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBarrierControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRDevice device, IsNSObject queue) => mtrClusterBarrierControl -> device -> CUShort -> queue -> IO (Id MTRClusterBarrierControl)
initWithDevice_endpoint_queue mtrClusterBarrierControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBarrierControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBarrierControl (mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBarrierControl (mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBarrierControl (mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBarrierControl -> device -> endpointID -> queue -> IO (Id MTRClusterBarrierControl)
initWithDevice_endpointID_queue mtrClusterBarrierControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBarrierControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @barrierControlStopWithExpectedValues:expectedValueInterval:completion:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector :: Selector
barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithParams:@
readAttributeBarrierMovingStateWithParamsSelector :: Selector
readAttributeBarrierMovingStateWithParamsSelector = mkSelector "readAttributeBarrierMovingStateWithParams:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithParams:@
readAttributeBarrierSafetyStatusWithParamsSelector :: Selector
readAttributeBarrierSafetyStatusWithParamsSelector = mkSelector "readAttributeBarrierSafetyStatusWithParams:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithParams:@
readAttributeBarrierCapabilitiesWithParamsSelector :: Selector
readAttributeBarrierCapabilitiesWithParamsSelector = mkSelector "readAttributeBarrierCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithParams:@
readAttributeBarrierOpenEventsWithParamsSelector :: Selector
readAttributeBarrierOpenEventsWithParamsSelector = mkSelector "readAttributeBarrierOpenEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithParams:@
readAttributeBarrierCloseEventsWithParamsSelector :: Selector
readAttributeBarrierCloseEventsWithParamsSelector = mkSelector "readAttributeBarrierCloseEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithParams:@
readAttributeBarrierCommandOpenEventsWithParamsSelector :: Selector
readAttributeBarrierCommandOpenEventsWithParamsSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithParams:@
readAttributeBarrierCommandCloseEventsWithParamsSelector :: Selector
readAttributeBarrierCommandCloseEventsWithParamsSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithParams:@
readAttributeBarrierOpenPeriodWithParamsSelector :: Selector
readAttributeBarrierOpenPeriodWithParamsSelector = mkSelector "readAttributeBarrierOpenPeriodWithParams:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithParams:@
readAttributeBarrierClosePeriodWithParamsSelector :: Selector
readAttributeBarrierClosePeriodWithParamsSelector = mkSelector "readAttributeBarrierClosePeriodWithParams:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:@
writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierPositionWithParams:@
readAttributeBarrierPositionWithParamsSelector :: Selector
readAttributeBarrierPositionWithParamsSelector = mkSelector "readAttributeBarrierPositionWithParams:"

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

-- | @Selector@ for @barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

