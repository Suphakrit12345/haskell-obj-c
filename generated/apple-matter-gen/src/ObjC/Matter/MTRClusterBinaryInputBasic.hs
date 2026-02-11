{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binary Input (Basic)    An interface for reading the value of a binary measurement and accessing various characteristics of that measurement.
--
-- Generated bindings for @MTRClusterBinaryInputBasic@.
module ObjC.Matter.MTRClusterBinaryInputBasic
  ( MTRClusterBinaryInputBasic
  , IsMTRClusterBinaryInputBasic(..)
  , readAttributeActiveTextWithParams
  , writeAttributeActiveTextWithValue_expectedValueInterval
  , writeAttributeActiveTextWithValue_expectedValueInterval_params
  , readAttributeDescriptionWithParams
  , writeAttributeDescriptionWithValue_expectedValueInterval
  , writeAttributeDescriptionWithValue_expectedValueInterval_params
  , readAttributeInactiveTextWithParams
  , writeAttributeInactiveTextWithValue_expectedValueInterval
  , writeAttributeInactiveTextWithValue_expectedValueInterval_params
  , readAttributeOutOfServiceWithParams
  , writeAttributeOutOfServiceWithValue_expectedValueInterval
  , writeAttributeOutOfServiceWithValue_expectedValueInterval_params
  , readAttributePolarityWithParams
  , readAttributePresentValueWithParams
  , writeAttributePresentValueWithValue_expectedValueInterval
  , writeAttributePresentValueWithValue_expectedValueInterval_params
  , readAttributeReliabilityWithParams
  , writeAttributeReliabilityWithValue_expectedValueInterval
  , writeAttributeReliabilityWithValue_expectedValueInterval_params
  , readAttributeStatusFlagsWithParams
  , readAttributeApplicationTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeActiveTextWithParamsSelector
  , writeAttributeActiveTextWithValue_expectedValueIntervalSelector
  , writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector
  , readAttributeDescriptionWithParamsSelector
  , writeAttributeDescriptionWithValue_expectedValueIntervalSelector
  , writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector
  , readAttributeInactiveTextWithParamsSelector
  , writeAttributeInactiveTextWithValue_expectedValueIntervalSelector
  , writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector
  , readAttributeOutOfServiceWithParamsSelector
  , writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector
  , writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector
  , readAttributePolarityWithParamsSelector
  , readAttributePresentValueWithParamsSelector
  , writeAttributePresentValueWithValue_expectedValueIntervalSelector
  , writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector
  , readAttributeReliabilityWithParamsSelector
  , writeAttributeReliabilityWithValue_expectedValueIntervalSelector
  , writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector
  , readAttributeStatusFlagsWithParamsSelector
  , readAttributeApplicationTypeWithParamsSelector
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

-- | @- readAttributeActiveTextWithParams:@
readAttributeActiveTextWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeActiveTextWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeActiveTextWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeActiveTextWithValue:expectedValueInterval:@
writeAttributeActiveTextWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveTextWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeActiveTextWithValue:expectedValueInterval:params:@
writeAttributeActiveTextWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveTextWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeDescriptionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDescriptionWithValue:expectedValueInterval:@
writeAttributeDescriptionWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDescriptionWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDescriptionWithValue:expectedValueInterval:params:@
writeAttributeDescriptionWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDescriptionWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeInactiveTextWithParams:@
readAttributeInactiveTextWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeInactiveTextWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeInactiveTextWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeInactiveTextWithValue:expectedValueInterval:@
writeAttributeInactiveTextWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeInactiveTextWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeInactiveTextWithValue:expectedValueInterval:params:@
writeAttributeInactiveTextWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeInactiveTextWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOutOfServiceWithParams:@
readAttributeOutOfServiceWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeOutOfServiceWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeOutOfServiceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOutOfServiceWithValue:expectedValueInterval:@
writeAttributeOutOfServiceWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOutOfServiceWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOutOfServiceWithValue:expectedValueInterval:params:@
writeAttributeOutOfServiceWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOutOfServiceWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePolarityWithParams:@
readAttributePolarityWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributePolarityWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributePolarityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePresentValueWithParams:@
readAttributePresentValueWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributePresentValueWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributePresentValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePresentValueWithValue:expectedValueInterval:@
writeAttributePresentValueWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePresentValueWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePresentValueWithValue:expectedValueInterval:params:@
writeAttributePresentValueWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePresentValueWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeReliabilityWithParams:@
readAttributeReliabilityWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeReliabilityWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeReliabilityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeReliabilityWithValue:expectedValueInterval:@
writeAttributeReliabilityWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeReliabilityWithValue_expectedValueInterval mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeReliabilityWithValue:expectedValueInterval:params:@
writeAttributeReliabilityWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeReliabilityWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStatusFlagsWithParams:@
readAttributeStatusFlagsWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeStatusFlagsWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeStatusFlagsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApplicationTypeWithParams:@
readAttributeApplicationTypeWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationTypeWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeApplicationTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBinaryInputBasic  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinaryInputBasic (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic => mtrClusterBinaryInputBasic -> IO (Id MTRClusterBinaryInputBasic)
init_ mtrClusterBinaryInputBasic  =
    sendMsg mtrClusterBinaryInputBasic (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBinaryInputBasic)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBinaryInputBasic"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterBinaryInputBasic -> device -> CUShort -> queue -> IO (Id MTRClusterBinaryInputBasic)
initWithDevice_endpoint_queue mtrClusterBinaryInputBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBinaryInputBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBinaryInputBasic -> device -> endpointID -> queue -> IO (Id MTRClusterBinaryInputBasic)
initWithDevice_endpointID_queue mtrClusterBinaryInputBasic  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBinaryInputBasic (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveTextWithParams:@
readAttributeActiveTextWithParamsSelector :: Selector
readAttributeActiveTextWithParamsSelector = mkSelector "readAttributeActiveTextWithParams:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:expectedValueInterval:@
writeAttributeActiveTextWithValue_expectedValueIntervalSelector :: Selector
writeAttributeActiveTextWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:expectedValueInterval:params:@
writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:expectedValueInterval:@
writeAttributeDescriptionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDescriptionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:expectedValueInterval:params:@
writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeInactiveTextWithParams:@
readAttributeInactiveTextWithParamsSelector :: Selector
readAttributeInactiveTextWithParamsSelector = mkSelector "readAttributeInactiveTextWithParams:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:expectedValueInterval:@
writeAttributeInactiveTextWithValue_expectedValueIntervalSelector :: Selector
writeAttributeInactiveTextWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:expectedValueInterval:params:@
writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOutOfServiceWithParams:@
readAttributeOutOfServiceWithParamsSelector :: Selector
readAttributeOutOfServiceWithParamsSelector = mkSelector "readAttributeOutOfServiceWithParams:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:expectedValueInterval:@
writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:expectedValueInterval:params:@
writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePolarityWithParams:@
readAttributePolarityWithParamsSelector :: Selector
readAttributePolarityWithParamsSelector = mkSelector "readAttributePolarityWithParams:"

-- | @Selector@ for @readAttributePresentValueWithParams:@
readAttributePresentValueWithParamsSelector :: Selector
readAttributePresentValueWithParamsSelector = mkSelector "readAttributePresentValueWithParams:"

-- | @Selector@ for @writeAttributePresentValueWithValue:expectedValueInterval:@
writeAttributePresentValueWithValue_expectedValueIntervalSelector :: Selector
writeAttributePresentValueWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePresentValueWithValue:expectedValueInterval:params:@
writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReliabilityWithParams:@
readAttributeReliabilityWithParamsSelector :: Selector
readAttributeReliabilityWithParamsSelector = mkSelector "readAttributeReliabilityWithParams:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:expectedValueInterval:@
writeAttributeReliabilityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeReliabilityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:expectedValueInterval:params:@
writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStatusFlagsWithParams:@
readAttributeStatusFlagsWithParamsSelector :: Selector
readAttributeStatusFlagsWithParamsSelector = mkSelector "readAttributeStatusFlagsWithParams:"

-- | @Selector@ for @readAttributeApplicationTypeWithParams:@
readAttributeApplicationTypeWithParamsSelector :: Selector
readAttributeApplicationTypeWithParamsSelector = mkSelector "readAttributeApplicationTypeWithParams:"

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

