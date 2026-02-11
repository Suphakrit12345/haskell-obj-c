{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Washer Controls    This cluster supports remotely monitoring and controlling the different types of functionality available to a washing device, such as a washing machine.
--
-- Generated bindings for @MTRClusterLaundryWasherControls@.
module ObjC.Matter.MTRClusterLaundryWasherControls
  ( MTRClusterLaundryWasherControls
  , IsMTRClusterLaundryWasherControls(..)
  , readAttributeSpinSpeedsWithParams
  , readAttributeSpinSpeedCurrentWithParams
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params
  , readAttributeNumberOfRinsesWithParams
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params
  , readAttributeSupportedRinsesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeSpinSpeedsWithParamsSelector
  , readAttributeSpinSpeedCurrentWithParamsSelector
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector
  , readAttributeNumberOfRinsesWithParamsSelector
  , writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedRinsesWithParamsSelector
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

-- | @- readAttributeSpinSpeedsWithParams:@
readAttributeSpinSpeedsWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSpinSpeedsWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeSpinSpeedsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpinSpeedCurrentWithParams:@
readAttributeSpinSpeedCurrentWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSpinSpeedCurrentWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeSpinSpeedCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval mtrClusterLaundryWasherControls  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLaundryWasherControls (mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params mtrClusterLaundryWasherControls  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLaundryWasherControls (mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeNumberOfRinsesWithParams:@
readAttributeNumberOfRinsesWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeNumberOfRinsesWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeNumberOfRinsesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeNumberOfRinsesWithValue:expectedValueInterval:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNumberOfRinsesWithValue_expectedValueInterval mtrClusterLaundryWasherControls  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLaundryWasherControls (mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params mtrClusterLaundryWasherControls  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterLaundryWasherControls (mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedRinsesWithParams:@
readAttributeSupportedRinsesWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSupportedRinsesWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeSupportedRinsesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLaundryWasherControls  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLaundryWasherControls (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls => mtrClusterLaundryWasherControls -> IO (Id MTRClusterLaundryWasherControls)
init_ mtrClusterLaundryWasherControls  =
    sendMsg mtrClusterLaundryWasherControls (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterLaundryWasherControls)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLaundryWasherControls"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLaundryWasherControls -> device -> endpointID -> queue -> IO (Id MTRClusterLaundryWasherControls)
initWithDevice_endpointID_queue mtrClusterLaundryWasherControls  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterLaundryWasherControls (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSpinSpeedsWithParams:@
readAttributeSpinSpeedsWithParamsSelector :: Selector
readAttributeSpinSpeedsWithParamsSelector = mkSelector "readAttributeSpinSpeedsWithParams:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithParams:@
readAttributeSpinSpeedCurrentWithParamsSelector :: Selector
readAttributeSpinSpeedCurrentWithParamsSelector = mkSelector "readAttributeSpinSpeedCurrentWithParams:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithParams:@
readAttributeNumberOfRinsesWithParamsSelector :: Selector
readAttributeNumberOfRinsesWithParamsSelector = mkSelector "readAttributeNumberOfRinsesWithParams:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:expectedValueInterval:@
writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector :: Selector
writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedRinsesWithParams:@
readAttributeSupportedRinsesWithParamsSelector :: Selector
readAttributeSupportedRinsesWithParamsSelector = mkSelector "readAttributeSupportedRinsesWithParams:"

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

