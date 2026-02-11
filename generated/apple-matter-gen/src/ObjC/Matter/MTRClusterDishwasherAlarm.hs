{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Dishwasher Alarm    Attributes and commands for configuring the Dishwasher alarm.
--
-- Generated bindings for @MTRClusterDishwasherAlarm@.
module ObjC.Matter.MTRClusterDishwasherAlarm
  ( MTRClusterDishwasherAlarm
  , IsMTRClusterDishwasherAlarm(..)
  , resetWithParams_expectedValues_expectedValueInterval_completion
  , modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaskWithParams
  , readAttributeLatchWithParams
  , readAttributeStateWithParams
  , readAttributeSupportedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , resetWithParams_expectedValues_expectedValueInterval_completionSelector
  , modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaskWithParamsSelector
  , readAttributeLatchWithParamsSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupportedWithParamsSelector
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

-- | @- resetWithParams:expectedValues:expectedValueInterval:completion:@
resetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterResetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDishwasherAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWithParams_expectedValues_expectedValueInterval_completion mtrClusterDishwasherAlarm  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDishwasherAlarm (mkSelector "resetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:@
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterModifyEnabledAlarmsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDishwasherAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion mtrClusterDishwasherAlarm  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDishwasherAlarm (mkSelector "modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaskWithParams:@
readAttributeMaskWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeMaskWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeMaskWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLatchWithParams:@
readAttributeLatchWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeLatchWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeLatchWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedWithParams:@
readAttributeSupportedWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeSupportedWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDishwasherAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDishwasherAlarm (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm => mtrClusterDishwasherAlarm -> IO (Id MTRClusterDishwasherAlarm)
init_ mtrClusterDishwasherAlarm  =
    sendMsg mtrClusterDishwasherAlarm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterDishwasherAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDishwasherAlarm"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDishwasherAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterDishwasherAlarm)
initWithDevice_endpointID_queue mtrClusterDishwasherAlarm  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterDishwasherAlarm (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWithParams:expectedValues:expectedValueInterval:completion:@
resetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:@
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaskWithParams:@
readAttributeMaskWithParamsSelector :: Selector
readAttributeMaskWithParamsSelector = mkSelector "readAttributeMaskWithParams:"

-- | @Selector@ for @readAttributeLatchWithParams:@
readAttributeLatchWithParamsSelector :: Selector
readAttributeLatchWithParamsSelector = mkSelector "readAttributeLatchWithParams:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupportedWithParams:@
readAttributeSupportedWithParamsSelector :: Selector
readAttributeSupportedWithParamsSelector = mkSelector "readAttributeSupportedWithParams:"

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

