{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Localization Configuration    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing common languages, units of measurements, and numerical formatting      standards. As such, Nodes that visually or audibly convey information need a mechanism by which      they can be configured to use a user’s preferred language, units, etc
--
-- Generated bindings for @MTRClusterLocalizationConfiguration@.
module ObjC.Matter.MTRClusterLocalizationConfiguration
  ( MTRClusterLocalizationConfiguration
  , IsMTRClusterLocalizationConfiguration(..)
  , readAttributeActiveLocaleWithParams
  , writeAttributeActiveLocaleWithValue_expectedValueInterval
  , writeAttributeActiveLocaleWithValue_expectedValueInterval_params
  , readAttributeSupportedLocalesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeActiveLocaleWithParamsSelector
  , writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector
  , writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedLocalesWithParamsSelector
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

-- | @- readAttributeActiveLocaleWithParams:@
readAttributeActiveLocaleWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeActiveLocaleWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeActiveLocaleWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeActiveLocaleWithValue:expectedValueInterval:@
writeAttributeActiveLocaleWithValue_expectedValueInterval :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLocalizationConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveLocaleWithValue_expectedValueInterval mtrClusterLocalizationConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLocalizationConfiguration (mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeActiveLocaleWithValue:expectedValueInterval:params:@
writeAttributeActiveLocaleWithValue_expectedValueInterval_params :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLocalizationConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveLocaleWithValue_expectedValueInterval_params mtrClusterLocalizationConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLocalizationConfiguration (mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedLocalesWithParams:@
readAttributeSupportedLocalesWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeSupportedLocalesWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeSupportedLocalesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLocalizationConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLocalizationConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration => mtrClusterLocalizationConfiguration -> IO (Id MTRClusterLocalizationConfiguration)
init_ mtrClusterLocalizationConfiguration  =
    sendMsg mtrClusterLocalizationConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterLocalizationConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLocalizationConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterLocalizationConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpoint_queue mtrClusterLocalizationConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterLocalizationConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLocalizationConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpointID_queue mtrClusterLocalizationConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterLocalizationConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveLocaleWithParams:@
readAttributeActiveLocaleWithParamsSelector :: Selector
readAttributeActiveLocaleWithParamsSelector = mkSelector "readAttributeActiveLocaleWithParams:"

-- | @Selector@ for @writeAttributeActiveLocaleWithValue:expectedValueInterval:@
writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector :: Selector
writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveLocaleWithValue:expectedValueInterval:params:@
writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedLocalesWithParams:@
readAttributeSupportedLocalesWithParamsSelector :: Selector
readAttributeSupportedLocalesWithParamsSelector = mkSelector "readAttributeSupportedLocalesWithParams:"

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

