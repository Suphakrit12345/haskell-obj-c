{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster User Label    The User Label Cluster provides a feature to tag an endpoint with zero or more labels.
--
-- Generated bindings for @MTRClusterUserLabel@.
module ObjC.Matter.MTRClusterUserLabel
  ( MTRClusterUserLabel
  , IsMTRClusterUserLabel(..)
  , readAttributeLabelListWithParams
  , writeAttributeLabelListWithValue_expectedValueInterval
  , writeAttributeLabelListWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeLabelListWithParamsSelector
  , writeAttributeLabelListWithValue_expectedValueIntervalSelector
  , writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeLabelListWithParams:@
readAttributeLabelListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeLabelListWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeLabelListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLabelListWithValue:expectedValueInterval:@
writeAttributeLabelListWithValue_expectedValueInterval :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterUserLabel -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLabelListWithValue_expectedValueInterval mtrClusterUserLabel  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterUserLabel (mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLabelListWithValue:expectedValueInterval:params:@
writeAttributeLabelListWithValue_expectedValueInterval_params :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterUserLabel -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLabelListWithValue_expectedValueInterval_params mtrClusterUserLabel  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterUserLabel (mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterUserLabel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterUserLabel (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterUserLabel mtrClusterUserLabel => mtrClusterUserLabel -> IO (Id MTRClusterUserLabel)
init_ mtrClusterUserLabel  =
    sendMsg mtrClusterUserLabel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterUserLabel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterUserLabel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRDevice device, IsNSObject queue) => mtrClusterUserLabel -> device -> CUShort -> queue -> IO (Id MTRClusterUserLabel)
initWithDevice_endpoint_queue mtrClusterUserLabel  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterUserLabel (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterUserLabel -> device -> endpointID -> queue -> IO (Id MTRClusterUserLabel)
initWithDevice_endpointID_queue mtrClusterUserLabel  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterUserLabel (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLabelListWithParams:@
readAttributeLabelListWithParamsSelector :: Selector
readAttributeLabelListWithParamsSelector = mkSelector "readAttributeLabelListWithParams:"

-- | @Selector@ for @writeAttributeLabelListWithValue:expectedValueInterval:@
writeAttributeLabelListWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLabelListWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLabelListWithValue:expectedValueInterval:params:@
writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:params:"

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

