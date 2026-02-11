{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fan Control    An interface for controlling a fan in a heating/cooling system.
--
-- Generated bindings for @MTRClusterFanControl@.
module ObjC.Matter.MTRClusterFanControl
  ( MTRClusterFanControl
  , IsMTRClusterFanControl(..)
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeFanModeWithParams
  , writeAttributeFanModeWithValue_expectedValueInterval
  , writeAttributeFanModeWithValue_expectedValueInterval_params
  , readAttributeFanModeSequenceWithParams
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval_params
  , readAttributePercentSettingWithParams
  , writeAttributePercentSettingWithValue_expectedValueInterval
  , writeAttributePercentSettingWithValue_expectedValueInterval_params
  , readAttributePercentCurrentWithParams
  , readAttributeSpeedMaxWithParams
  , readAttributeSpeedSettingWithParams
  , writeAttributeSpeedSettingWithValue_expectedValueInterval
  , writeAttributeSpeedSettingWithValue_expectedValueInterval_params
  , readAttributeSpeedCurrentWithParams
  , readAttributeRockSupportWithParams
  , readAttributeRockSettingWithParams
  , writeAttributeRockSettingWithValue_expectedValueInterval
  , writeAttributeRockSettingWithValue_expectedValueInterval_params
  , readAttributeWindSupportWithParams
  , readAttributeWindSettingWithParams
  , writeAttributeWindSettingWithValue_expectedValueInterval
  , writeAttributeWindSettingWithValue_expectedValueInterval_params
  , readAttributeAirflowDirectionWithParams
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeFanModeWithParamsSelector
  , writeAttributeFanModeWithValue_expectedValueIntervalSelector
  , writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeFanModeSequenceWithParamsSelector
  , writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector
  , readAttributePercentSettingWithParamsSelector
  , writeAttributePercentSettingWithValue_expectedValueIntervalSelector
  , writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector
  , readAttributePercentCurrentWithParamsSelector
  , readAttributeSpeedMaxWithParamsSelector
  , readAttributeSpeedSettingWithParamsSelector
  , writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector
  , writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector
  , readAttributeSpeedCurrentWithParamsSelector
  , readAttributeRockSupportWithParamsSelector
  , readAttributeRockSettingWithParamsSelector
  , writeAttributeRockSettingWithValue_expectedValueIntervalSelector
  , writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector
  , readAttributeWindSupportWithParamsSelector
  , readAttributeWindSettingWithParamsSelector
  , writeAttributeWindSettingWithValue_expectedValueIntervalSelector
  , writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector
  , readAttributeAirflowDirectionWithParamsSelector
  , writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
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

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRFanControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterFanControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterFanControl (mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFanModeWithParams:@
readAttributeFanModeWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFanModeWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeFanModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeFanModeWithValue:expectedValueInterval:@
writeAttributeFanModeWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFanModeWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeFanModeWithValue:expectedValueInterval:params:@
writeAttributeFanModeWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFanModeWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeFanModeSequenceWithParams:@
readAttributeFanModeSequenceWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFanModeSequenceWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeFanModeSequenceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeFanModeSequenceWithValue:expectedValueInterval:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFanModeSequenceWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFanModeSequenceWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePercentSettingWithParams:@
readAttributePercentSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributePercentSettingWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributePercentSettingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePercentSettingWithValue:expectedValueInterval:@
writeAttributePercentSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePercentSettingWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePercentSettingWithValue:expectedValueInterval:params:@
writeAttributePercentSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePercentSettingWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePercentCurrentWithParams:@
readAttributePercentCurrentWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributePercentCurrentWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributePercentCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpeedMaxWithParams:@
readAttributeSpeedMaxWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedMaxWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeSpeedMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpeedSettingWithParams:@
readAttributeSpeedSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedSettingWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeSpeedSettingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSpeedSettingWithValue:expectedValueInterval:@
writeAttributeSpeedSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpeedSettingWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSpeedSettingWithValue:expectedValueInterval:params:@
writeAttributeSpeedSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpeedSettingWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSpeedCurrentWithParams:@
readAttributeSpeedCurrentWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedCurrentWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeSpeedCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRockSupportWithParams:@
readAttributeRockSupportWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeRockSupportWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeRockSupportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRockSettingWithParams:@
readAttributeRockSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeRockSettingWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeRockSettingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRockSettingWithValue:expectedValueInterval:@
writeAttributeRockSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRockSettingWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRockSettingWithValue:expectedValueInterval:params:@
writeAttributeRockSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRockSettingWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeWindSupportWithParams:@
readAttributeWindSupportWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeWindSupportWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeWindSupportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWindSettingWithParams:@
readAttributeWindSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeWindSettingWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeWindSettingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeWindSettingWithValue:expectedValueInterval:@
writeAttributeWindSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeWindSettingWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeWindSettingWithValue:expectedValueInterval:params:@
writeAttributeWindSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeWindSettingWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAirflowDirectionWithParams:@
readAttributeAirflowDirectionWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAirflowDirectionWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeAirflowDirectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAirflowDirectionWithValue:expectedValueInterval:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAirflowDirectionWithValue_expectedValueInterval mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterFanControl (mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAirflowDirectionWithValue_expectedValueInterval_params mtrClusterFanControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterFanControl (mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFanControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterFanControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterFanControl mtrClusterFanControl => mtrClusterFanControl -> IO (Id MTRClusterFanControl)
init_ mtrClusterFanControl  =
    sendMsg mtrClusterFanControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterFanControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFanControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRDevice device, IsNSObject queue) => mtrClusterFanControl -> device -> CUShort -> queue -> IO (Id MTRClusterFanControl)
initWithDevice_endpoint_queue mtrClusterFanControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterFanControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFanControl -> device -> endpointID -> queue -> IO (Id MTRClusterFanControl)
initWithDevice_endpointID_queue mtrClusterFanControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterFanControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeFanModeWithParams:@
readAttributeFanModeWithParamsSelector :: Selector
readAttributeFanModeWithParamsSelector = mkSelector "readAttributeFanModeWithParams:"

-- | @Selector@ for @writeAttributeFanModeWithValue:expectedValueInterval:@
writeAttributeFanModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeFanModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFanModeWithValue:expectedValueInterval:params:@
writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeFanModeSequenceWithParams:@
readAttributeFanModeSequenceWithParamsSelector :: Selector
readAttributeFanModeSequenceWithParamsSelector = mkSelector "readAttributeFanModeSequenceWithParams:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:expectedValueInterval:@
writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector :: Selector
writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePercentSettingWithParams:@
readAttributePercentSettingWithParamsSelector :: Selector
readAttributePercentSettingWithParamsSelector = mkSelector "readAttributePercentSettingWithParams:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:expectedValueInterval:@
writeAttributePercentSettingWithValue_expectedValueIntervalSelector :: Selector
writeAttributePercentSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:expectedValueInterval:params:@
writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePercentCurrentWithParams:@
readAttributePercentCurrentWithParamsSelector :: Selector
readAttributePercentCurrentWithParamsSelector = mkSelector "readAttributePercentCurrentWithParams:"

-- | @Selector@ for @readAttributeSpeedMaxWithParams:@
readAttributeSpeedMaxWithParamsSelector :: Selector
readAttributeSpeedMaxWithParamsSelector = mkSelector "readAttributeSpeedMaxWithParams:"

-- | @Selector@ for @readAttributeSpeedSettingWithParams:@
readAttributeSpeedSettingWithParamsSelector :: Selector
readAttributeSpeedSettingWithParamsSelector = mkSelector "readAttributeSpeedSettingWithParams:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:expectedValueInterval:@
writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:expectedValueInterval:params:@
writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSpeedCurrentWithParams:@
readAttributeSpeedCurrentWithParamsSelector :: Selector
readAttributeSpeedCurrentWithParamsSelector = mkSelector "readAttributeSpeedCurrentWithParams:"

-- | @Selector@ for @readAttributeRockSupportWithParams:@
readAttributeRockSupportWithParamsSelector :: Selector
readAttributeRockSupportWithParamsSelector = mkSelector "readAttributeRockSupportWithParams:"

-- | @Selector@ for @readAttributeRockSettingWithParams:@
readAttributeRockSettingWithParamsSelector :: Selector
readAttributeRockSettingWithParamsSelector = mkSelector "readAttributeRockSettingWithParams:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:expectedValueInterval:@
writeAttributeRockSettingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRockSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:expectedValueInterval:params:@
writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeWindSupportWithParams:@
readAttributeWindSupportWithParamsSelector :: Selector
readAttributeWindSupportWithParamsSelector = mkSelector "readAttributeWindSupportWithParams:"

-- | @Selector@ for @readAttributeWindSettingWithParams:@
readAttributeWindSettingWithParamsSelector :: Selector
readAttributeWindSettingWithParamsSelector = mkSelector "readAttributeWindSettingWithParams:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:expectedValueInterval:@
writeAttributeWindSettingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeWindSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:expectedValueInterval:params:@
writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAirflowDirectionWithParams:@
readAttributeAirflowDirectionWithParamsSelector :: Selector
readAttributeAirflowDirectionWithParamsSelector = mkSelector "readAttributeAirflowDirectionWithParams:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:expectedValueInterval:@
writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

