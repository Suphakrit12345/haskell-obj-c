{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Format Localization    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for how dates and times are conveyed. As such, Nodes that visually      or audibly convey time information need a mechanism by which they can be configured to use a      user’s preferred format.
--
-- Generated bindings for @MTRClusterTimeFormatLocalization@.
module ObjC.Matter.MTRClusterTimeFormatLocalization
  ( MTRClusterTimeFormatLocalization
  , IsMTRClusterTimeFormatLocalization(..)
  , readAttributeHourFormatWithParams
  , writeAttributeHourFormatWithValue_expectedValueInterval
  , writeAttributeHourFormatWithValue_expectedValueInterval_params
  , readAttributeActiveCalendarTypeWithParams
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params
  , readAttributeSupportedCalendarTypesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeHourFormatWithParamsSelector
  , writeAttributeHourFormatWithValue_expectedValueIntervalSelector
  , writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector
  , readAttributeActiveCalendarTypeWithParamsSelector
  , writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedCalendarTypesWithParamsSelector
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

-- | @- readAttributeHourFormatWithParams:@
readAttributeHourFormatWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeHourFormatWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeHourFormatWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeHourFormatWithValue:expectedValueInterval:@
writeAttributeHourFormatWithValue_expectedValueInterval :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeHourFormatWithValue_expectedValueInterval mtrClusterTimeFormatLocalization  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeHourFormatWithValue:expectedValueInterval:params:@
writeAttributeHourFormatWithValue_expectedValueInterval_params :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeHourFormatWithValue_expectedValueInterval_params mtrClusterTimeFormatLocalization  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeActiveCalendarTypeWithParams:@
readAttributeActiveCalendarTypeWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeActiveCalendarTypeWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeActiveCalendarTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval mtrClusterTimeFormatLocalization  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params mtrClusterTimeFormatLocalization  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedCalendarTypesWithParams:@
readAttributeSupportedCalendarTypesWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeSupportedCalendarTypesWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeSupportedCalendarTypesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimeFormatLocalization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeFormatLocalization (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization => mtrClusterTimeFormatLocalization -> IO (Id MTRClusterTimeFormatLocalization)
init_ mtrClusterTimeFormatLocalization  =
    sendMsg mtrClusterTimeFormatLocalization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTimeFormatLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimeFormatLocalization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRDevice device, IsNSObject queue) => mtrClusterTimeFormatLocalization -> device -> CUShort -> queue -> IO (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpoint_queue mtrClusterTimeFormatLocalization  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterTimeFormatLocalization (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimeFormatLocalization -> device -> endpointID -> queue -> IO (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpointID_queue mtrClusterTimeFormatLocalization  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTimeFormatLocalization (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeHourFormatWithParams:@
readAttributeHourFormatWithParamsSelector :: Selector
readAttributeHourFormatWithParamsSelector = mkSelector "readAttributeHourFormatWithParams:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:expectedValueInterval:@
writeAttributeHourFormatWithValue_expectedValueIntervalSelector :: Selector
writeAttributeHourFormatWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:expectedValueInterval:params:@
writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithParams:@
readAttributeActiveCalendarTypeWithParamsSelector :: Selector
readAttributeActiveCalendarTypeWithParamsSelector = mkSelector "readAttributeActiveCalendarTypeWithParams:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:@
writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithParams:@
readAttributeSupportedCalendarTypesWithParamsSelector :: Selector
readAttributeSupportedCalendarTypesWithParamsSelector = mkSelector "readAttributeSupportedCalendarTypesWithParams:"

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

