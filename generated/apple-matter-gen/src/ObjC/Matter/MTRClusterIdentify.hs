{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Identify    Attributes and commands for putting a device into Identification mode (e.g. flashing a light).
--
-- Generated bindings for @MTRClusterIdentify@.
module ObjC.Matter.MTRClusterIdentify
  ( MTRClusterIdentify
  , IsMTRClusterIdentify(..)
  , identifyWithParams_expectedValues_expectedValueInterval_completion
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeIdentifyTimeWithParams
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval_params
  , readAttributeIdentifyTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , identifyWithParams_expectedValues_expectedValueInterval_completionHandler
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , identifyWithParams_expectedValues_expectedValueInterval_completionSelector
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeIdentifyTimeWithParamsSelector
  , writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeIdentifyTypeWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- identifyWithParams:expectedValues:expectedValueInterval:completion:@
identifyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterIdentifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
identifyWithParams_expectedValues_expectedValueInterval_completion mtrClusterIdentify  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterIdentify (mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- triggerEffectWithParams:expectedValues:expectedValueInterval:completion:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completion mtrClusterIdentify  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterIdentify (mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIdentifyTimeWithParams:@
readAttributeIdentifyTimeWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeIdentifyTimeWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeIdentifyTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeIdentifyTimeWithValue:expectedValueInterval:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval :: (IsMTRClusterIdentify mtrClusterIdentify, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIdentifyTimeWithValue_expectedValueInterval mtrClusterIdentify  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterIdentify (mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval_params :: (IsMTRClusterIdentify mtrClusterIdentify, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterIdentify -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIdentifyTimeWithValue_expectedValueInterval_params mtrClusterIdentify  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterIdentify (mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeIdentifyTypeWithParams:@
readAttributeIdentifyTypeWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeIdentifyTypeWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeIdentifyTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterIdentify  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIdentify (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterIdentify mtrClusterIdentify => mtrClusterIdentify -> IO (Id MTRClusterIdentify)
init_ mtrClusterIdentify  =
    sendMsg mtrClusterIdentify (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterIdentify)
new  =
  do
    cls' <- getRequiredClass "MTRClusterIdentify"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRDevice device, IsNSObject queue) => mtrClusterIdentify -> device -> CUShort -> queue -> IO (Id MTRClusterIdentify)
initWithDevice_endpoint_queue mtrClusterIdentify  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterIdentify (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifyWithParams:expectedValues:expectedValueInterval:completionHandler:@
identifyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterIdentifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
identifyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterIdentify  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterIdentify (mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterIdentify  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterIdentify (mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterIdentify -> device -> endpointID -> queue -> IO (Id MTRClusterIdentify)
initWithDevice_endpointID_queue mtrClusterIdentify  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterIdentify (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifyWithParams:expectedValues:expectedValueInterval:completion:@
identifyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
identifyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @triggerEffectWithParams:expectedValues:expectedValueInterval:completion:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeIdentifyTimeWithParams:@
readAttributeIdentifyTimeWithParamsSelector :: Selector
readAttributeIdentifyTimeWithParamsSelector = mkSelector "readAttributeIdentifyTimeWithParams:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:expectedValueInterval:@
writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeIdentifyTypeWithParams:@
readAttributeIdentifyTypeWithParamsSelector :: Selector
readAttributeIdentifyTypeWithParamsSelector = mkSelector "readAttributeIdentifyTypeWithParams:"

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

-- | @Selector@ for @identifyWithParams:expectedValues:expectedValueInterval:completionHandler:@
identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

