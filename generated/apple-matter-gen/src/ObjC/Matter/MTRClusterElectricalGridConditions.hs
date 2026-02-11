{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Grid Conditions    The Electrical Grid Conditions Cluster provides the mechanism for communicating electricity grid carbon intensity to devices within the premises in units of Grams of CO2e per kWh.
--
-- Generated bindings for @MTRClusterElectricalGridConditions@.
module ObjC.Matter.MTRClusterElectricalGridConditions
  ( MTRClusterElectricalGridConditions
  , IsMTRClusterElectricalGridConditions(..)
  , readAttributeLocalGenerationAvailableWithParams
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params
  , readAttributeCurrentConditionsWithParams
  , readAttributeForecastConditionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeLocalGenerationAvailableWithParamsSelector
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector
  , readAttributeCurrentConditionsWithParamsSelector
  , readAttributeForecastConditionsWithParamsSelector
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

-- | @- readAttributeLocalGenerationAvailableWithParams:@
readAttributeLocalGenerationAvailableWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeLocalGenerationAvailableWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeLocalGenerationAvailableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalGridConditions -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval mtrClusterElectricalGridConditions  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalGridConditions (mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalGridConditions -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params mtrClusterElectricalGridConditions  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalGridConditions (mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeCurrentConditionsWithParams:@
readAttributeCurrentConditionsWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeCurrentConditionsWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeCurrentConditionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeForecastConditionsWithParams:@
readAttributeForecastConditionsWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeForecastConditionsWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeForecastConditionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalGridConditions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalGridConditions (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions => mtrClusterElectricalGridConditions -> IO (Id MTRClusterElectricalGridConditions)
init_ mtrClusterElectricalGridConditions  =
    sendMsg mtrClusterElectricalGridConditions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterElectricalGridConditions)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalGridConditions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalGridConditions -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalGridConditions)
initWithDevice_endpointID_queue mtrClusterElectricalGridConditions  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterElectricalGridConditions (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithParams:@
readAttributeLocalGenerationAvailableWithParamsSelector :: Selector
readAttributeLocalGenerationAvailableWithParamsSelector = mkSelector "readAttributeLocalGenerationAvailableWithParams:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeCurrentConditionsWithParams:@
readAttributeCurrentConditionsWithParamsSelector :: Selector
readAttributeCurrentConditionsWithParamsSelector = mkSelector "readAttributeCurrentConditionsWithParams:"

-- | @Selector@ for @readAttributeForecastConditionsWithParams:@
readAttributeForecastConditionsWithParamsSelector :: Selector
readAttributeForecastConditionsWithParamsSelector = mkSelector "readAttributeForecastConditionsWithParams:"

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

