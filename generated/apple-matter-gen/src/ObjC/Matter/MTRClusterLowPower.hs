{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Low Power    This cluster provides an interface for managing low power mode on a device.
--
-- Generated bindings for @MTRClusterLowPower@.
module ObjC.Matter.MTRClusterLowPower
  ( MTRClusterLowPower
  , IsMTRClusterLowPower(..)
  , sleepWithParams_expectedValues_expectedValueInterval_completion
  , sleepWithExpectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , sleepWithParams_expectedValues_expectedValueInterval_completionHandler
  , sleepWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , sleepWithParams_expectedValues_expectedValueInterval_completionSelector
  , sleepWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- sleepWithParams:expectedValues:expectedValueInterval:completion:@
sleepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRLowPowerClusterSleepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithParams_expectedValues_expectedValueInterval_completion mtrClusterLowPower  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLowPower (mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- sleepWithExpectedValues:expectedValueInterval:completion:@
sleepWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterLowPower mtrClusterLowPower, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithExpectedValues_expectedValueInterval_completion mtrClusterLowPower  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLowPower (mkSelector "sleepWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLowPower  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLowPower (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLowPower  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLowPower (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLowPower  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLowPower (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLowPower  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLowPower (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLowPower  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterLowPower (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterLowPower mtrClusterLowPower => mtrClusterLowPower -> IO (Id MTRClusterLowPower)
init_ mtrClusterLowPower  =
    sendMsg mtrClusterLowPower (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterLowPower)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLowPower"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRDevice device, IsNSObject queue) => mtrClusterLowPower -> device -> CUShort -> queue -> IO (Id MTRClusterLowPower)
initWithDevice_endpoint_queue mtrClusterLowPower  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterLowPower (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- sleepWithParams:expectedValues:expectedValueInterval:completionHandler:@
sleepWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRLowPowerClusterSleepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLowPower  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterLowPower (mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- sleepWithExpectedValues:expectedValueInterval:completionHandler:@
sleepWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLowPower mtrClusterLowPower, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithExpectedValues_expectedValueInterval_completionHandler mtrClusterLowPower  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterLowPower (mkSelector "sleepWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLowPower -> device -> endpointID -> queue -> IO (Id MTRClusterLowPower)
initWithDevice_endpointID_queue mtrClusterLowPower  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterLowPower (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepWithParams:expectedValues:expectedValueInterval:completion:@
sleepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
sleepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @sleepWithExpectedValues:expectedValueInterval:completion:@
sleepWithExpectedValues_expectedValueInterval_completionSelector :: Selector
sleepWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "sleepWithExpectedValues:expectedValueInterval:completion:"

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

-- | @Selector@ for @sleepWithParams:expectedValues:expectedValueInterval:completionHandler:@
sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @sleepWithExpectedValues:expectedValueInterval:completionHandler:@
sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sleepWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

