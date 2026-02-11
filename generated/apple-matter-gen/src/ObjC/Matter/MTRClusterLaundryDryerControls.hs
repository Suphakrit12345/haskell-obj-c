{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Dryer Controls    This cluster provides a way to access options associated with the operation of            a laundry dryer device type.
--
-- Generated bindings for @MTRClusterLaundryDryerControls@.
module ObjC.Matter.MTRClusterLaundryDryerControls
  ( MTRClusterLaundryDryerControls
  , IsMTRClusterLaundryDryerControls(..)
  , readAttributeSupportedDrynessLevelsWithParams
  , readAttributeSelectedDrynessLevelWithParams
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeSupportedDrynessLevelsWithParamsSelector
  , readAttributeSelectedDrynessLevelWithParamsSelector
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeSupportedDrynessLevelsWithParams:@
readAttributeSupportedDrynessLevelsWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeSupportedDrynessLevelsWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeSupportedDrynessLevelsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSelectedDrynessLevelWithParams:@
readAttributeSelectedDrynessLevelWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeSelectedDrynessLevelWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeSelectedDrynessLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryDryerControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval mtrClusterLaundryDryerControls  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLaundryDryerControls (mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryDryerControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params mtrClusterLaundryDryerControls  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLaundryDryerControls (mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLaundryDryerControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryDryerControls (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls => mtrClusterLaundryDryerControls -> IO (Id MTRClusterLaundryDryerControls)
init_ mtrClusterLaundryDryerControls  =
    sendMsg mtrClusterLaundryDryerControls (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterLaundryDryerControls)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLaundryDryerControls"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLaundryDryerControls -> device -> endpointID -> queue -> IO (Id MTRClusterLaundryDryerControls)
initWithDevice_endpointID_queue mtrClusterLaundryDryerControls  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterLaundryDryerControls (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithParams:@
readAttributeSupportedDrynessLevelsWithParamsSelector :: Selector
readAttributeSupportedDrynessLevelsWithParamsSelector = mkSelector "readAttributeSupportedDrynessLevelsWithParams:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithParams:@
readAttributeSelectedDrynessLevelWithParamsSelector :: Selector
readAttributeSelectedDrynessLevelWithParamsSelector = mkSelector "readAttributeSelectedDrynessLevelWithParams:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:"

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

