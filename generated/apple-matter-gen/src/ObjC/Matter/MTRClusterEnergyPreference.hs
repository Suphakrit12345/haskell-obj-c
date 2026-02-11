{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy Preference    This cluster provides an interface to specify preferences for how devices should consume energy.
--
-- Generated bindings for @MTRClusterEnergyPreference@.
module ObjC.Matter.MTRClusterEnergyPreference
  ( MTRClusterEnergyPreference
  , IsMTRClusterEnergyPreference(..)
  , readAttributeEnergyBalancesWithParams
  , readAttributeCurrentEnergyBalanceWithParams
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params
  , readAttributeEnergyPrioritiesWithParams
  , readAttributeLowPowerModeSensitivitiesWithParams
  , readAttributeCurrentLowPowerModeSensitivityWithParams
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeEnergyBalancesWithParamsSelector
  , readAttributeCurrentEnergyBalanceWithParamsSelector
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector
  , readAttributeEnergyPrioritiesWithParamsSelector
  , readAttributeLowPowerModeSensitivitiesWithParamsSelector
  , readAttributeCurrentLowPowerModeSensitivityWithParamsSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeEnergyBalancesWithParams:@
readAttributeEnergyBalancesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeEnergyBalancesWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeEnergyBalancesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentEnergyBalanceWithParams:@
readAttributeCurrentEnergyBalanceWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeCurrentEnergyBalanceWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeCurrentEnergyBalanceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval mtrClusterEnergyPreference  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyPreference (mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params mtrClusterEnergyPreference  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterEnergyPreference (mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeEnergyPrioritiesWithParams:@
readAttributeEnergyPrioritiesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeEnergyPrioritiesWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeEnergyPrioritiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLowPowerModeSensitivitiesWithParams:@
readAttributeLowPowerModeSensitivitiesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeLowPowerModeSensitivitiesWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeLowPowerModeSensitivitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentLowPowerModeSensitivityWithParams:@
readAttributeCurrentLowPowerModeSensitivityWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeCurrentLowPowerModeSensitivityWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeCurrentLowPowerModeSensitivityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval mtrClusterEnergyPreference  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyPreference (mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params mtrClusterEnergyPreference  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterEnergyPreference (mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEnergyPreference  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyPreference (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterEnergyPreference mtrClusterEnergyPreference => mtrClusterEnergyPreference -> IO (Id MTRClusterEnergyPreference)
init_ mtrClusterEnergyPreference  =
    sendMsg mtrClusterEnergyPreference (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterEnergyPreference)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEnergyPreference"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEnergyPreference -> device -> endpointID -> queue -> IO (Id MTRClusterEnergyPreference)
initWithDevice_endpointID_queue mtrClusterEnergyPreference  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterEnergyPreference (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeEnergyBalancesWithParams:@
readAttributeEnergyBalancesWithParamsSelector :: Selector
readAttributeEnergyBalancesWithParamsSelector = mkSelector "readAttributeEnergyBalancesWithParams:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithParams:@
readAttributeCurrentEnergyBalanceWithParamsSelector :: Selector
readAttributeCurrentEnergyBalanceWithParamsSelector = mkSelector "readAttributeCurrentEnergyBalanceWithParams:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector :: Selector
writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithParams:@
readAttributeEnergyPrioritiesWithParamsSelector :: Selector
readAttributeEnergyPrioritiesWithParamsSelector = mkSelector "readAttributeEnergyPrioritiesWithParams:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithParams:@
readAttributeLowPowerModeSensitivitiesWithParamsSelector :: Selector
readAttributeLowPowerModeSensitivitiesWithParamsSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithParams:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithParams:@
readAttributeCurrentLowPowerModeSensitivityWithParamsSelector :: Selector
readAttributeCurrentLowPowerModeSensitivityWithParamsSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithParams:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:"

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

