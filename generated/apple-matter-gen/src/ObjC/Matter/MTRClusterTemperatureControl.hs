{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Control    Attributes and commands for configuring the temperature control, and reporting temperature.
--
-- Generated bindings for @MTRClusterTemperatureControl@.
module ObjC.Matter.MTRClusterTemperatureControl
  ( MTRClusterTemperatureControl
  , IsMTRClusterTemperatureControl(..)
  , setTemperatureWithParams_expectedValues_expectedValueInterval_completion
  , setTemperatureWithExpectedValues_expectedValueInterval_completion
  , readAttributeTemperatureSetpointWithParams
  , readAttributeMinTemperatureWithParams
  , readAttributeMaxTemperatureWithParams
  , readAttributeStepWithParams
  , readAttributeSelectedTemperatureLevelWithParams
  , readAttributeSupportedTemperatureLevelsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTemperatureWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeTemperatureSetpointWithParamsSelector
  , readAttributeMinTemperatureWithParamsSelector
  , readAttributeMaxTemperatureWithParamsSelector
  , readAttributeStepWithParamsSelector
  , readAttributeSelectedTemperatureLevelWithParamsSelector
  , readAttributeSupportedTemperatureLevelsWithParamsSelector
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

-- | @- setTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
setTemperatureWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRTemperatureControlClusterSetTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTemperatureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTemperatureWithParams_expectedValues_expectedValueInterval_completion mtrClusterTemperatureControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTemperatureControl (mkSelector "setTemperatureWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTemperatureWithExpectedValues:expectedValueInterval:completion:@
setTemperatureWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTemperatureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setTemperatureWithExpectedValues_expectedValueInterval_completion mtrClusterTemperatureControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTemperatureControl (mkSelector "setTemperatureWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTemperatureSetpointWithParams:@
readAttributeTemperatureSetpointWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeTemperatureSetpointWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeTemperatureSetpointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinTemperatureWithParams:@
readAttributeMinTemperatureWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeMinTemperatureWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeMinTemperatureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxTemperatureWithParams:@
readAttributeMaxTemperatureWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeMaxTemperatureWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeMaxTemperatureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStepWithParams:@
readAttributeStepWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeStepWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeStepWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSelectedTemperatureLevelWithParams:@
readAttributeSelectedTemperatureLevelWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeSelectedTemperatureLevelWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeSelectedTemperatureLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedTemperatureLevelsWithParams:@
readAttributeSupportedTemperatureLevelsWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeSupportedTemperatureLevelsWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeSupportedTemperatureLevelsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTemperatureControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTemperatureControl mtrClusterTemperatureControl => mtrClusterTemperatureControl -> IO (Id MTRClusterTemperatureControl)
init_ mtrClusterTemperatureControl  =
    sendMsg mtrClusterTemperatureControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTemperatureControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTemperatureControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTemperatureControl -> device -> endpointID -> queue -> IO (Id MTRClusterTemperatureControl)
initWithDevice_endpointID_queue mtrClusterTemperatureControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTemperatureControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTemperatureWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTemperatureWithExpectedValues:expectedValueInterval:completion:@
setTemperatureWithExpectedValues_expectedValueInterval_completionSelector :: Selector
setTemperatureWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setTemperatureWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithParams:@
readAttributeTemperatureSetpointWithParamsSelector :: Selector
readAttributeTemperatureSetpointWithParamsSelector = mkSelector "readAttributeTemperatureSetpointWithParams:"

-- | @Selector@ for @readAttributeMinTemperatureWithParams:@
readAttributeMinTemperatureWithParamsSelector :: Selector
readAttributeMinTemperatureWithParamsSelector = mkSelector "readAttributeMinTemperatureWithParams:"

-- | @Selector@ for @readAttributeMaxTemperatureWithParams:@
readAttributeMaxTemperatureWithParamsSelector :: Selector
readAttributeMaxTemperatureWithParamsSelector = mkSelector "readAttributeMaxTemperatureWithParams:"

-- | @Selector@ for @readAttributeStepWithParams:@
readAttributeStepWithParamsSelector :: Selector
readAttributeStepWithParamsSelector = mkSelector "readAttributeStepWithParams:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithParams:@
readAttributeSelectedTemperatureLevelWithParamsSelector :: Selector
readAttributeSelectedTemperatureLevelWithParamsSelector = mkSelector "readAttributeSelectedTemperatureLevelWithParams:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithParams:@
readAttributeSupportedTemperatureLevelsWithParamsSelector :: Selector
readAttributeSupportedTemperatureLevelsWithParamsSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithParams:"

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

