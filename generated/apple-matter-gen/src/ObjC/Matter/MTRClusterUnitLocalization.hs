{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Unit Localization    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for the units in which values are conveyed in communication to a      user. As such, Nodes that visually or audibly convey measurable values to the user need a      mechanism by which they can be configured to use a user’s preferred unit.
--
-- Generated bindings for @MTRClusterUnitLocalization@.
module ObjC.Matter.MTRClusterUnitLocalization
  ( MTRClusterUnitLocalization
  , IsMTRClusterUnitLocalization(..)
  , readAttributeTemperatureUnitWithParams
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval_params
  , readAttributeSupportedTemperatureUnitsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeTemperatureUnitWithParamsSelector
  , writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedTemperatureUnitsWithParamsSelector
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

-- | @- readAttributeTemperatureUnitWithParams:@
readAttributeTemperatureUnitWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeTemperatureUnitWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeTemperatureUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeTemperatureUnitWithValue:expectedValueInterval:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterUnitLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureUnitWithValue_expectedValueInterval mtrClusterUnitLocalization  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterUnitLocalization (mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval_params :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterUnitLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureUnitWithValue_expectedValueInterval_params mtrClusterUnitLocalization  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterUnitLocalization (mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedTemperatureUnitsWithParams:@
readAttributeSupportedTemperatureUnitsWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeSupportedTemperatureUnitsWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeSupportedTemperatureUnitsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterUnitLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUnitLocalization (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterUnitLocalization mtrClusterUnitLocalization => mtrClusterUnitLocalization -> IO (Id MTRClusterUnitLocalization)
init_ mtrClusterUnitLocalization  =
    sendMsg mtrClusterUnitLocalization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterUnitLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterUnitLocalization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRDevice device, IsNSObject queue) => mtrClusterUnitLocalization -> device -> CUShort -> queue -> IO (Id MTRClusterUnitLocalization)
initWithDevice_endpoint_queue mtrClusterUnitLocalization  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterUnitLocalization (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterUnitLocalization -> device -> endpointID -> queue -> IO (Id MTRClusterUnitLocalization)
initWithDevice_endpointID_queue mtrClusterUnitLocalization  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterUnitLocalization (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeTemperatureUnitWithParams:@
readAttributeTemperatureUnitWithParamsSelector :: Selector
readAttributeTemperatureUnitWithParamsSelector = mkSelector "readAttributeTemperatureUnitWithParams:"

-- | @Selector@ for @writeAttributeTemperatureUnitWithValue:expectedValueInterval:@
writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedTemperatureUnitsWithParams:@
readAttributeSupportedTemperatureUnitsWithParamsSelector :: Selector
readAttributeSupportedTemperatureUnitsWithParamsSelector = mkSelector "readAttributeSupportedTemperatureUnitsWithParams:"

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

