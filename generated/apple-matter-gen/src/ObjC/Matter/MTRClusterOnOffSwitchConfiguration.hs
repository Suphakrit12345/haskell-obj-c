{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/off Switch Configuration    Attributes and commands for configuring On/Off switching devices.
--
-- Generated bindings for @MTRClusterOnOffSwitchConfiguration@.
module ObjC.Matter.MTRClusterOnOffSwitchConfiguration
  ( MTRClusterOnOffSwitchConfiguration
  , IsMTRClusterOnOffSwitchConfiguration(..)
  , readAttributeSwitchTypeWithParams
  , readAttributeSwitchActionsWithParams
  , writeAttributeSwitchActionsWithValue_expectedValueInterval
  , writeAttributeSwitchActionsWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeSwitchTypeWithParamsSelector
  , readAttributeSwitchActionsWithParamsSelector
  , writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector
  , writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeSwitchTypeWithParams:@
readAttributeSwitchTypeWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeSwitchTypeWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSwitchActionsWithParams:@
readAttributeSwitchActionsWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeSwitchActionsWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchActionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSwitchActionsWithValue:expectedValueInterval:@
writeAttributeSwitchActionsWithValue_expectedValueInterval :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOffSwitchConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSwitchActionsWithValue_expectedValueInterval mtrClusterOnOffSwitchConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSwitchActionsWithValue:expectedValueInterval:params:@
writeAttributeSwitchActionsWithValue_expectedValueInterval_params :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOffSwitchConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSwitchActionsWithValue_expectedValueInterval_params mtrClusterOnOffSwitchConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOnOffSwitchConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration => mtrClusterOnOffSwitchConfiguration -> IO (Id MTRClusterOnOffSwitchConfiguration)
init_ mtrClusterOnOffSwitchConfiguration  =
    sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOnOffSwitchConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOnOffSwitchConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterOnOffSwitchConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpoint_queue mtrClusterOnOffSwitchConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOnOffSwitchConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpointID_queue mtrClusterOnOffSwitchConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOnOffSwitchConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSwitchTypeWithParams:@
readAttributeSwitchTypeWithParamsSelector :: Selector
readAttributeSwitchTypeWithParamsSelector = mkSelector "readAttributeSwitchTypeWithParams:"

-- | @Selector@ for @readAttributeSwitchActionsWithParams:@
readAttributeSwitchActionsWithParamsSelector :: Selector
readAttributeSwitchActionsWithParamsSelector = mkSelector "readAttributeSwitchActionsWithParams:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:expectedValueInterval:@
writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:expectedValueInterval:params:@
writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:params:"

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

