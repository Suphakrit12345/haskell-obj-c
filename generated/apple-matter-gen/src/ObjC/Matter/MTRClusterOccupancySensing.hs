{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Occupancy Sensing    The server cluster provides an interface to occupancy sensing functionality based on one or more sensing modalities, including configuration and provision of notifications of occupancy status.
--
-- Generated bindings for @MTRClusterOccupancySensing@.
module ObjC.Matter.MTRClusterOccupancySensing
  ( MTRClusterOccupancySensing
  , IsMTRClusterOccupancySensing(..)
  , readAttributeOccupancyWithParams
  , readAttributeOccupancySensorTypeWithParams
  , readAttributeOccupancySensorTypeBitmapWithParams
  , readAttributeHoldTimeWithParams
  , writeAttributeHoldTimeWithValue_expectedValueInterval
  , writeAttributeHoldTimeWithValue_expectedValueInterval_params
  , readAttributeHoldTimeLimitsWithParams
  , readAttributePIROccupiedToUnoccupiedDelayWithParams
  , writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval
  , writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePIRUnoccupiedToOccupiedDelayWithParams
  , writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval
  , writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePIRUnoccupiedToOccupiedThresholdWithParams
  , writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval
  , writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params
  , readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams
  , writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval
  , writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params
  , readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams
  , writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval
  , writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params
  , readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams
  , writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval
  , writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params
  , readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams
  , writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval
  , writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams
  , writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval
  , writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams
  , writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval
  , writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributePirOccupiedToUnoccupiedDelayWithParams
  , writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval
  , writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePirUnoccupiedToOccupiedDelayWithParams
  , writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval
  , writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params
  , readAttributePirUnoccupiedToOccupiedThresholdWithParams
  , writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval
  , writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params
  , initWithDevice_endpointID_queue
  , readAttributeOccupancyWithParamsSelector
  , readAttributeOccupancySensorTypeWithParamsSelector
  , readAttributeOccupancySensorTypeBitmapWithParamsSelector
  , readAttributeHoldTimeWithParamsSelector
  , writeAttributeHoldTimeWithValue_expectedValueIntervalSelector
  , writeAttributeHoldTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeHoldTimeLimitsWithParamsSelector
  , readAttributePIROccupiedToUnoccupiedDelayWithParamsSelector
  , writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePIRUnoccupiedToOccupiedDelayWithParamsSelector
  , writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePIRUnoccupiedToOccupiedThresholdWithParamsSelector
  , writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector
  , writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector
  , readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParamsSelector
  , writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParamsSelector
  , writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParamsSelector
  , writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector
  , writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector
  , readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParamsSelector
  , writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParamsSelector
  , writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParamsSelector
  , writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector
  , writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , readAttributePirOccupiedToUnoccupiedDelayWithParamsSelector
  , writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePirUnoccupiedToOccupiedDelayWithParamsSelector
  , writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector
  , writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector
  , readAttributePirUnoccupiedToOccupiedThresholdWithParamsSelector
  , writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector
  , writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeOccupancyWithParams:@
readAttributeOccupancyWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeOccupancyWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeOccupancyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOccupancySensorTypeWithParams:@
readAttributeOccupancySensorTypeWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeOccupancySensorTypeWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeOccupancySensorTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOccupancySensorTypeBitmapWithParams:@
readAttributeOccupancySensorTypeBitmapWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeOccupancySensorTypeBitmapWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeOccupancySensorTypeBitmapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHoldTimeWithParams:@
readAttributeHoldTimeWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeHoldTimeWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeHoldTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeHoldTimeWithValue:expectedValueInterval:@
writeAttributeHoldTimeWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeHoldTimeWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeHoldTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeHoldTimeWithValue:expectedValueInterval:params:@
writeAttributeHoldTimeWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeHoldTimeWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeHoldTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeHoldTimeLimitsWithParams:@
readAttributeHoldTimeLimitsWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeHoldTimeLimitsWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeHoldTimeLimitsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePIROccupiedToUnoccupiedDelayWithParams:@
readAttributePIROccupiedToUnoccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePIROccupiedToUnoccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePIROccupiedToUnoccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePIRUnoccupiedToOccupiedDelayWithParams:@
readAttributePIRUnoccupiedToOccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePIRUnoccupiedToOccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePIRUnoccupiedToOccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePIRUnoccupiedToOccupiedThresholdWithParams:@
readAttributePIRUnoccupiedToOccupiedThresholdWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePIRUnoccupiedToOccupiedThresholdWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePIRUnoccupiedToOccupiedThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams:@
readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams:@
readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams:@
readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams:@
readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams:@
readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams:@
readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOccupancySensing mtrClusterOccupancySensing => mtrClusterOccupancySensing -> IO (Id MTRClusterOccupancySensing)
init_ mtrClusterOccupancySensing  =
    sendMsg mtrClusterOccupancySensing (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOccupancySensing)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOccupancySensing"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRDevice device, IsNSObject queue) => mtrClusterOccupancySensing -> device -> CUShort -> queue -> IO (Id MTRClusterOccupancySensing)
initWithDevice_endpoint_queue mtrClusterOccupancySensing  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOccupancySensing (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributePirOccupiedToUnoccupiedDelayWithParams:@
readAttributePirOccupiedToUnoccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePirOccupiedToUnoccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePirOccupiedToUnoccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePirUnoccupiedToOccupiedDelayWithParams:@
readAttributePirUnoccupiedToOccupiedDelayWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePirUnoccupiedToOccupiedDelayWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePirUnoccupiedToOccupiedDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePirUnoccupiedToOccupiedThresholdWithParams:@
readAttributePirUnoccupiedToOccupiedThresholdWithParams :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRReadParams params) => mtrClusterOccupancySensing -> params -> IO (Id NSDictionary)
readAttributePirUnoccupiedToOccupiedThresholdWithParams mtrClusterOccupancySensing  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOccupancySensing (mkSelector "readAttributePirUnoccupiedToOccupiedThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOccupancySensing -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_params mtrClusterOccupancySensing  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOccupancySensing (mkSelector "writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOccupancySensing mtrClusterOccupancySensing, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOccupancySensing -> device -> endpointID -> queue -> IO (Id MTRClusterOccupancySensing)
initWithDevice_endpointID_queue mtrClusterOccupancySensing  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOccupancySensing (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeOccupancyWithParams:@
readAttributeOccupancyWithParamsSelector :: Selector
readAttributeOccupancyWithParamsSelector = mkSelector "readAttributeOccupancyWithParams:"

-- | @Selector@ for @readAttributeOccupancySensorTypeWithParams:@
readAttributeOccupancySensorTypeWithParamsSelector :: Selector
readAttributeOccupancySensorTypeWithParamsSelector = mkSelector "readAttributeOccupancySensorTypeWithParams:"

-- | @Selector@ for @readAttributeOccupancySensorTypeBitmapWithParams:@
readAttributeOccupancySensorTypeBitmapWithParamsSelector :: Selector
readAttributeOccupancySensorTypeBitmapWithParamsSelector = mkSelector "readAttributeOccupancySensorTypeBitmapWithParams:"

-- | @Selector@ for @readAttributeHoldTimeWithParams:@
readAttributeHoldTimeWithParamsSelector :: Selector
readAttributeHoldTimeWithParamsSelector = mkSelector "readAttributeHoldTimeWithParams:"

-- | @Selector@ for @writeAttributeHoldTimeWithValue:expectedValueInterval:@
writeAttributeHoldTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeHoldTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeHoldTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeHoldTimeWithValue:expectedValueInterval:params:@
writeAttributeHoldTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeHoldTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeHoldTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeHoldTimeLimitsWithParams:@
readAttributeHoldTimeLimitsWithParamsSelector :: Selector
readAttributeHoldTimeLimitsWithParamsSelector = mkSelector "readAttributeHoldTimeLimitsWithParams:"

-- | @Selector@ for @readAttributePIROccupiedToUnoccupiedDelayWithParams:@
readAttributePIROccupiedToUnoccupiedDelayWithParamsSelector :: Selector
readAttributePIROccupiedToUnoccupiedDelayWithParamsSelector = mkSelector "readAttributePIROccupiedToUnoccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePIROccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePIROccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePIRUnoccupiedToOccupiedDelayWithParams:@
readAttributePIRUnoccupiedToOccupiedDelayWithParamsSelector :: Selector
readAttributePIRUnoccupiedToOccupiedDelayWithParamsSelector = mkSelector "readAttributePIRUnoccupiedToOccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePIRUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePIRUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePIRUnoccupiedToOccupiedThresholdWithParams:@
readAttributePIRUnoccupiedToOccupiedThresholdWithParamsSelector :: Selector
readAttributePIRUnoccupiedToOccupiedThresholdWithParamsSelector = mkSelector "readAttributePIRUnoccupiedToOccupiedThresholdWithParams:"

-- | @Selector@ for @writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector :: Selector
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePIRUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePIRUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams:@
readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParamsSelector :: Selector
readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParamsSelector = mkSelector "readAttributeUltrasonicOccupiedToUnoccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUltrasonicOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams:@
readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParamsSelector :: Selector
readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParamsSelector = mkSelector "readAttributeUltrasonicUnoccupiedToOccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams:@
readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParamsSelector :: Selector
readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParamsSelector = mkSelector "readAttributeUltrasonicUnoccupiedToOccupiedThresholdWithParams:"

-- | @Selector@ for @writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUltrasonicUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams:@
readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParamsSelector :: Selector
readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParamsSelector = mkSelector "readAttributePhysicalContactOccupiedToUnoccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePhysicalContactOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams:@
readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParamsSelector :: Selector
readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParamsSelector = mkSelector "readAttributePhysicalContactUnoccupiedToOccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams:@
readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParamsSelector :: Selector
readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParamsSelector = mkSelector "readAttributePhysicalContactUnoccupiedToOccupiedThresholdWithParams:"

-- | @Selector@ for @writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector :: Selector
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePhysicalContactUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @readAttributePirOccupiedToUnoccupiedDelayWithParams:@
readAttributePirOccupiedToUnoccupiedDelayWithParamsSelector :: Selector
readAttributePirOccupiedToUnoccupiedDelayWithParamsSelector = mkSelector "readAttributePirOccupiedToUnoccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:@
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePirOccupiedToUnoccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePirOccupiedToUnoccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePirUnoccupiedToOccupiedDelayWithParams:@
readAttributePirUnoccupiedToOccupiedDelayWithParamsSelector :: Selector
readAttributePirUnoccupiedToOccupiedDelayWithParamsSelector = mkSelector "readAttributePirUnoccupiedToOccupiedDelayWithParams:"

-- | @Selector@ for @writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:@
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector :: Selector
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:@
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePirUnoccupiedToOccupiedDelayWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePirUnoccupiedToOccupiedDelayWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePirUnoccupiedToOccupiedThresholdWithParams:@
readAttributePirUnoccupiedToOccupiedThresholdWithParamsSelector :: Selector
readAttributePirUnoccupiedToOccupiedThresholdWithParamsSelector = mkSelector "readAttributePirUnoccupiedToOccupiedThresholdWithParams:"

-- | @Selector@ for @writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:@
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector :: Selector
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:@
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePirUnoccupiedToOccupiedThresholdWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePirUnoccupiedToOccupiedThresholdWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

