{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ballast Configuration    Attributes and commands for configuring a lighting ballast.
--
-- Generated bindings for @MTRClusterBallastConfiguration@.
module ObjC.Matter.MTRClusterBallastConfiguration
  ( MTRClusterBallastConfiguration
  , IsMTRClusterBallastConfiguration(..)
  , readAttributePhysicalMinLevelWithParams
  , readAttributePhysicalMaxLevelWithParams
  , readAttributeBallastStatusWithParams
  , readAttributeMinLevelWithParams
  , writeAttributeMinLevelWithValue_expectedValueInterval
  , writeAttributeMinLevelWithValue_expectedValueInterval_params
  , readAttributeMaxLevelWithParams
  , writeAttributeMaxLevelWithValue_expectedValueInterval
  , writeAttributeMaxLevelWithValue_expectedValueInterval_params
  , readAttributeIntrinsicBallastFactorWithParams
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params
  , readAttributeBallastFactorAdjustmentWithParams
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params
  , readAttributeLampQuantityWithParams
  , readAttributeLampTypeWithParams
  , writeAttributeLampTypeWithValue_expectedValueInterval
  , writeAttributeLampTypeWithValue_expectedValueInterval_params
  , readAttributeLampManufacturerWithParams
  , writeAttributeLampManufacturerWithValue_expectedValueInterval
  , writeAttributeLampManufacturerWithValue_expectedValueInterval_params
  , readAttributeLampRatedHoursWithParams
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval_params
  , readAttributeLampBurnHoursWithParams
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval_params
  , readAttributeLampAlarmModeWithParams
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval_params
  , readAttributeLampBurnHoursTripPointWithParams
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeIntrinsicBalanceFactorWithParams
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params
  , initWithDevice_endpointID_queue
  , readAttributePhysicalMinLevelWithParamsSelector
  , readAttributePhysicalMaxLevelWithParamsSelector
  , readAttributeBallastStatusWithParamsSelector
  , readAttributeMinLevelWithParamsSelector
  , writeAttributeMinLevelWithValue_expectedValueIntervalSelector
  , writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeMaxLevelWithParamsSelector
  , writeAttributeMaxLevelWithValue_expectedValueIntervalSelector
  , writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeIntrinsicBallastFactorWithParamsSelector
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector
  , readAttributeBallastFactorAdjustmentWithParamsSelector
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampQuantityWithParamsSelector
  , readAttributeLampTypeWithParamsSelector
  , writeAttributeLampTypeWithValue_expectedValueIntervalSelector
  , writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampManufacturerWithParamsSelector
  , writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector
  , writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampRatedHoursWithParamsSelector
  , writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampBurnHoursWithParamsSelector
  , writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampAlarmModeWithParamsSelector
  , writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeLampBurnHoursTripPointWithParamsSelector
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , readAttributeIntrinsicBalanceFactorWithParamsSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributePhysicalMinLevelWithParams:@
readAttributePhysicalMinLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributePhysicalMinLevelWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributePhysicalMinLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePhysicalMaxLevelWithParams:@
readAttributePhysicalMaxLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributePhysicalMaxLevelWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributePhysicalMaxLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBallastStatusWithParams:@
readAttributeBallastStatusWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeBallastStatusWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeBallastStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeMinLevelWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeMinLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMinLevelWithValue:expectedValueInterval:@
writeAttributeMinLevelWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMinLevelWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMinLevelWithValue:expectedValueInterval:params:@
writeAttributeMinLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMinLevelWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeMaxLevelWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeMaxLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMaxLevelWithValue:expectedValueInterval:@
writeAttributeMaxLevelWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMaxLevelWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMaxLevelWithValue:expectedValueInterval:params:@
writeAttributeMaxLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMaxLevelWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeIntrinsicBallastFactorWithParams:@
readAttributeIntrinsicBallastFactorWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeIntrinsicBallastFactorWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeIntrinsicBallastFactorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBallastFactorAdjustmentWithParams:@
readAttributeBallastFactorAdjustmentWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeBallastFactorAdjustmentWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeBallastFactorAdjustmentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampQuantityWithParams:@
readAttributeLampQuantityWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampQuantityWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampQuantityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLampTypeWithParams:@
readAttributeLampTypeWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampTypeWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampTypeWithValue:expectedValueInterval:@
writeAttributeLampTypeWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampTypeWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampTypeWithValue:expectedValueInterval:params:@
writeAttributeLampTypeWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampTypeWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampManufacturerWithParams:@
readAttributeLampManufacturerWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampManufacturerWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampManufacturerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampManufacturerWithValue:expectedValueInterval:@
writeAttributeLampManufacturerWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampManufacturerWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampManufacturerWithValue:expectedValueInterval:params:@
writeAttributeLampManufacturerWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampManufacturerWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampRatedHoursWithParams:@
readAttributeLampRatedHoursWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampRatedHoursWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampRatedHoursWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampRatedHoursWithValue:expectedValueInterval:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampRatedHoursWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampRatedHoursWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampBurnHoursWithParams:@
readAttributeLampBurnHoursWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampBurnHoursWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampBurnHoursWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampBurnHoursWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampBurnHoursWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampAlarmModeWithParams:@
readAttributeLampAlarmModeWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampAlarmModeWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampAlarmModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampAlarmModeWithValue:expectedValueInterval:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampAlarmModeWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampAlarmModeWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLampBurnHoursTripPointWithParams:@
readAttributeLampBurnHoursTripPointWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampBurnHoursTripPointWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursTripPointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration => mtrClusterBallastConfiguration -> IO (Id MTRClusterBallastConfiguration)
init_ mtrClusterBallastConfiguration  =
    sendMsg mtrClusterBallastConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBallastConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBallastConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterBallastConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterBallastConfiguration)
initWithDevice_endpoint_queue mtrClusterBallastConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeIntrinsicBalanceFactorWithParams:@
readAttributeIntrinsicBalanceFactorWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeIntrinsicBalanceFactorWithParams mtrClusterBallastConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBallastConfiguration (mkSelector "readAttributeIntrinsicBalanceFactorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params mtrClusterBallastConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBallastConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterBallastConfiguration)
initWithDevice_endpointID_queue mtrClusterBallastConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBallastConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePhysicalMinLevelWithParams:@
readAttributePhysicalMinLevelWithParamsSelector :: Selector
readAttributePhysicalMinLevelWithParamsSelector = mkSelector "readAttributePhysicalMinLevelWithParams:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithParams:@
readAttributePhysicalMaxLevelWithParamsSelector :: Selector
readAttributePhysicalMaxLevelWithParamsSelector = mkSelector "readAttributePhysicalMaxLevelWithParams:"

-- | @Selector@ for @readAttributeBallastStatusWithParams:@
readAttributeBallastStatusWithParamsSelector :: Selector
readAttributeBallastStatusWithParamsSelector = mkSelector "readAttributeBallastStatusWithParams:"

-- | @Selector@ for @readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParamsSelector :: Selector
readAttributeMinLevelWithParamsSelector = mkSelector "readAttributeMinLevelWithParams:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:expectedValueInterval:@
writeAttributeMinLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMinLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:expectedValueInterval:params:@
writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParamsSelector :: Selector
readAttributeMaxLevelWithParamsSelector = mkSelector "readAttributeMaxLevelWithParams:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:expectedValueInterval:@
writeAttributeMaxLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMaxLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:expectedValueInterval:params:@
writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeIntrinsicBallastFactorWithParams:@
readAttributeIntrinsicBallastFactorWithParamsSelector :: Selector
readAttributeIntrinsicBallastFactorWithParamsSelector = mkSelector "readAttributeIntrinsicBallastFactorWithParams:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector :: Selector
writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithParams:@
readAttributeBallastFactorAdjustmentWithParamsSelector :: Selector
readAttributeBallastFactorAdjustmentWithParamsSelector = mkSelector "readAttributeBallastFactorAdjustmentWithParams:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampQuantityWithParams:@
readAttributeLampQuantityWithParamsSelector :: Selector
readAttributeLampQuantityWithParamsSelector = mkSelector "readAttributeLampQuantityWithParams:"

-- | @Selector@ for @readAttributeLampTypeWithParams:@
readAttributeLampTypeWithParamsSelector :: Selector
readAttributeLampTypeWithParamsSelector = mkSelector "readAttributeLampTypeWithParams:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:expectedValueInterval:@
writeAttributeLampTypeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:expectedValueInterval:params:@
writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampManufacturerWithParams:@
readAttributeLampManufacturerWithParamsSelector :: Selector
readAttributeLampManufacturerWithParamsSelector = mkSelector "readAttributeLampManufacturerWithParams:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:expectedValueInterval:@
writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:expectedValueInterval:params:@
writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampRatedHoursWithParams:@
readAttributeLampRatedHoursWithParamsSelector :: Selector
readAttributeLampRatedHoursWithParamsSelector = mkSelector "readAttributeLampRatedHoursWithParams:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:expectedValueInterval:@
writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampBurnHoursWithParams:@
readAttributeLampBurnHoursWithParamsSelector :: Selector
readAttributeLampBurnHoursWithParamsSelector = mkSelector "readAttributeLampBurnHoursWithParams:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampAlarmModeWithParams:@
readAttributeLampAlarmModeWithParamsSelector :: Selector
readAttributeLampAlarmModeWithParamsSelector = mkSelector "readAttributeLampAlarmModeWithParams:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:expectedValueInterval:@
writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithParams:@
readAttributeLampBurnHoursTripPointWithParamsSelector :: Selector
readAttributeLampBurnHoursTripPointWithParamsSelector = mkSelector "readAttributeLampBurnHoursTripPointWithParams:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @readAttributeIntrinsicBalanceFactorWithParams:@
readAttributeIntrinsicBalanceFactorWithParamsSelector :: Selector
readAttributeIntrinsicBalanceFactorWithParamsSelector = mkSelector "readAttributeIntrinsicBalanceFactorWithParams:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector :: Selector
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

