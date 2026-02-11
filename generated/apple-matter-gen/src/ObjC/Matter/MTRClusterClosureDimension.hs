{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Dimension    This cluster provides an interface to reflect and control a closure's range of movement, usually involving a panel, by using 6-axis framework.
--
-- Generated bindings for @MTRClusterClosureDimension@.
module ObjC.Matter.MTRClusterClosureDimension
  ( MTRClusterClosureDimension
  , IsMTRClusterClosureDimension(..)
  , setTargetWithParams_expectedValues_expectedValueInterval_completion
  , setTargetWithExpectedValues_expectedValueInterval_completion
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentStateWithParams
  , readAttributeTargetStateWithParams
  , readAttributeResolutionWithParams
  , readAttributeStepValueWithParams
  , readAttributeUnitWithParams
  , readAttributeUnitRangeWithParams
  , readAttributeLimitRangeWithParams
  , readAttributeTranslationDirectionWithParams
  , readAttributeRotationAxisWithParams
  , readAttributeOverflowWithParams
  , readAttributeModulationTypeWithParams
  , readAttributeLatchControlModesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , setTargetWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTargetWithExpectedValues_expectedValueInterval_completionSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeTargetStateWithParamsSelector
  , readAttributeResolutionWithParamsSelector
  , readAttributeStepValueWithParamsSelector
  , readAttributeUnitWithParamsSelector
  , readAttributeUnitRangeWithParamsSelector
  , readAttributeLimitRangeWithParamsSelector
  , readAttributeTranslationDirectionWithParamsSelector
  , readAttributeRotationAxisWithParamsSelector
  , readAttributeOverflowWithParamsSelector
  , readAttributeModulationTypeWithParamsSelector
  , readAttributeLatchControlModesWithParamsSelector
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

-- | @- setTargetWithParams:expectedValues:expectedValueInterval:completion:@
setTargetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRClosureDimensionClusterSetTargetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureDimension  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterClosureDimension (mkSelector "setTargetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTargetWithExpectedValues:expectedValueInterval:completion:@
setTargetWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetWithExpectedValues_expectedValueInterval_completion mtrClusterClosureDimension  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterClosureDimension (mkSelector "setTargetWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRClosureDimensionClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureDimension  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterClosureDimension (mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeCurrentStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeTargetStateWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeTargetStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeResolutionWithParams:@
readAttributeResolutionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeResolutionWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeResolutionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStepValueWithParams:@
readAttributeStepValueWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeStepValueWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeStepValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUnitWithParams:@
readAttributeUnitWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeUnitWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeUnitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUnitRangeWithParams:@
readAttributeUnitRangeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeUnitRangeWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeUnitRangeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLimitRangeWithParams:@
readAttributeLimitRangeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeLimitRangeWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeLimitRangeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTranslationDirectionWithParams:@
readAttributeTranslationDirectionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeTranslationDirectionWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeTranslationDirectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRotationAxisWithParams:@
readAttributeRotationAxisWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeRotationAxisWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeRotationAxisWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverflowWithParams:@
readAttributeOverflowWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeOverflowWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeOverflowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeModulationTypeWithParams:@
readAttributeModulationTypeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeModulationTypeWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeModulationTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeLatchControlModesWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeLatchControlModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterClosureDimension  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterClosureDimension (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterClosureDimension mtrClusterClosureDimension => mtrClusterClosureDimension -> IO (Id MTRClusterClosureDimension)
init_ mtrClusterClosureDimension  =
    sendMsg mtrClusterClosureDimension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterClosureDimension)
new  =
  do
    cls' <- getRequiredClass "MTRClusterClosureDimension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterClosureDimension -> device -> endpointID -> queue -> IO (Id MTRClusterClosureDimension)
initWithDevice_endpointID_queue mtrClusterClosureDimension  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterClosureDimension (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTargetWithParams:expectedValues:expectedValueInterval:completion:@
setTargetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTargetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTargetWithExpectedValues:expectedValueInterval:completion:@
setTargetWithExpectedValues_expectedValueInterval_completionSelector :: Selector
setTargetWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParamsSelector :: Selector
readAttributeTargetStateWithParamsSelector = mkSelector "readAttributeTargetStateWithParams:"

-- | @Selector@ for @readAttributeResolutionWithParams:@
readAttributeResolutionWithParamsSelector :: Selector
readAttributeResolutionWithParamsSelector = mkSelector "readAttributeResolutionWithParams:"

-- | @Selector@ for @readAttributeStepValueWithParams:@
readAttributeStepValueWithParamsSelector :: Selector
readAttributeStepValueWithParamsSelector = mkSelector "readAttributeStepValueWithParams:"

-- | @Selector@ for @readAttributeUnitWithParams:@
readAttributeUnitWithParamsSelector :: Selector
readAttributeUnitWithParamsSelector = mkSelector "readAttributeUnitWithParams:"

-- | @Selector@ for @readAttributeUnitRangeWithParams:@
readAttributeUnitRangeWithParamsSelector :: Selector
readAttributeUnitRangeWithParamsSelector = mkSelector "readAttributeUnitRangeWithParams:"

-- | @Selector@ for @readAttributeLimitRangeWithParams:@
readAttributeLimitRangeWithParamsSelector :: Selector
readAttributeLimitRangeWithParamsSelector = mkSelector "readAttributeLimitRangeWithParams:"

-- | @Selector@ for @readAttributeTranslationDirectionWithParams:@
readAttributeTranslationDirectionWithParamsSelector :: Selector
readAttributeTranslationDirectionWithParamsSelector = mkSelector "readAttributeTranslationDirectionWithParams:"

-- | @Selector@ for @readAttributeRotationAxisWithParams:@
readAttributeRotationAxisWithParamsSelector :: Selector
readAttributeRotationAxisWithParamsSelector = mkSelector "readAttributeRotationAxisWithParams:"

-- | @Selector@ for @readAttributeOverflowWithParams:@
readAttributeOverflowWithParamsSelector :: Selector
readAttributeOverflowWithParamsSelector = mkSelector "readAttributeOverflowWithParams:"

-- | @Selector@ for @readAttributeModulationTypeWithParams:@
readAttributeModulationTypeWithParamsSelector :: Selector
readAttributeModulationTypeWithParamsSelector = mkSelector "readAttributeModulationTypeWithParams:"

-- | @Selector@ for @readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParamsSelector :: Selector
readAttributeLatchControlModesWithParamsSelector = mkSelector "readAttributeLatchControlModesWithParams:"

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

