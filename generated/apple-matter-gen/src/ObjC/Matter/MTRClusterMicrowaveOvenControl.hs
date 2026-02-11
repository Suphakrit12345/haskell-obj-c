{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Control    Attributes and commands for configuring the microwave oven control, and reporting cooking stats.
--
-- Generated bindings for @MTRClusterMicrowaveOvenControl@.
module ObjC.Matter.MTRClusterMicrowaveOvenControl
  ( MTRClusterMicrowaveOvenControl
  , IsMTRClusterMicrowaveOvenControl(..)
  , setCookingParametersWithParams_expectedValues_expectedValueInterval_completion
  , setCookingParametersWithExpectedValues_expectedValueInterval_completion
  , addMoreTimeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCookTimeWithParams
  , readAttributeMaxCookTimeWithParams
  , readAttributePowerSettingWithParams
  , readAttributeMinPowerWithParams
  , readAttributeMaxPowerWithParams
  , readAttributePowerStepWithParams
  , readAttributeSupportedWattsWithParams
  , readAttributeSelectedWattIndexWithParams
  , readAttributeWattRatingWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector
  , setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector
  , addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCookTimeWithParamsSelector
  , readAttributeMaxCookTimeWithParamsSelector
  , readAttributePowerSettingWithParamsSelector
  , readAttributeMinPowerWithParamsSelector
  , readAttributeMaxPowerWithParamsSelector
  , readAttributePowerStepWithParamsSelector
  , readAttributeSupportedWattsWithParamsSelector
  , readAttributeSelectedWattIndexWithParamsSelector
  , readAttributeWattRatingWithParamsSelector
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

-- | @- setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:@
setCookingParametersWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterSetCookingParametersParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setCookingParametersWithParams_expectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMicrowaveOvenControl (mkSelector "setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setCookingParametersWithExpectedValues:expectedValueInterval:completion:@
setCookingParametersWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setCookingParametersWithExpectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMicrowaveOvenControl (mkSelector "setCookingParametersWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:@
addMoreTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterAddMoreTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addMoreTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMicrowaveOvenControl (mkSelector "addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCookTimeWithParams:@
readAttributeCookTimeWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeCookTimeWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeCookTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxCookTimeWithParams:@
readAttributeMaxCookTimeWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMaxCookTimeWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeMaxCookTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerSettingWithParams:@
readAttributePowerSettingWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributePowerSettingWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributePowerSettingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinPowerWithParams:@
readAttributeMinPowerWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMinPowerWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeMinPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxPowerWithParams:@
readAttributeMaxPowerWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMaxPowerWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeMaxPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerStepWithParams:@
readAttributePowerStepWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributePowerStepWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributePowerStepWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedWattsWithParams:@
readAttributeSupportedWattsWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeSupportedWattsWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeSupportedWattsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSelectedWattIndexWithParams:@
readAttributeSelectedWattIndexWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeSelectedWattIndexWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeSelectedWattIndexWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWattRatingWithParams:@
readAttributeWattRatingWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeWattRatingWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeWattRatingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMicrowaveOvenControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl => mtrClusterMicrowaveOvenControl -> IO (Id MTRClusterMicrowaveOvenControl)
init_ mtrClusterMicrowaveOvenControl  =
    sendMsg mtrClusterMicrowaveOvenControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMicrowaveOvenControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMicrowaveOvenControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMicrowaveOvenControl -> device -> endpointID -> queue -> IO (Id MTRClusterMicrowaveOvenControl)
initWithDevice_endpointID_queue mtrClusterMicrowaveOvenControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMicrowaveOvenControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:@
setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setCookingParametersWithExpectedValues:expectedValueInterval:completion:@
setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector :: Selector
setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setCookingParametersWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:@
addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCookTimeWithParams:@
readAttributeCookTimeWithParamsSelector :: Selector
readAttributeCookTimeWithParamsSelector = mkSelector "readAttributeCookTimeWithParams:"

-- | @Selector@ for @readAttributeMaxCookTimeWithParams:@
readAttributeMaxCookTimeWithParamsSelector :: Selector
readAttributeMaxCookTimeWithParamsSelector = mkSelector "readAttributeMaxCookTimeWithParams:"

-- | @Selector@ for @readAttributePowerSettingWithParams:@
readAttributePowerSettingWithParamsSelector :: Selector
readAttributePowerSettingWithParamsSelector = mkSelector "readAttributePowerSettingWithParams:"

-- | @Selector@ for @readAttributeMinPowerWithParams:@
readAttributeMinPowerWithParamsSelector :: Selector
readAttributeMinPowerWithParamsSelector = mkSelector "readAttributeMinPowerWithParams:"

-- | @Selector@ for @readAttributeMaxPowerWithParams:@
readAttributeMaxPowerWithParamsSelector :: Selector
readAttributeMaxPowerWithParamsSelector = mkSelector "readAttributeMaxPowerWithParams:"

-- | @Selector@ for @readAttributePowerStepWithParams:@
readAttributePowerStepWithParamsSelector :: Selector
readAttributePowerStepWithParamsSelector = mkSelector "readAttributePowerStepWithParams:"

-- | @Selector@ for @readAttributeSupportedWattsWithParams:@
readAttributeSupportedWattsWithParamsSelector :: Selector
readAttributeSupportedWattsWithParamsSelector = mkSelector "readAttributeSupportedWattsWithParams:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithParams:@
readAttributeSelectedWattIndexWithParamsSelector :: Selector
readAttributeSelectedWattIndexWithParamsSelector = mkSelector "readAttributeSelectedWattIndexWithParams:"

-- | @Selector@ for @readAttributeWattRatingWithParams:@
readAttributeWattRatingWithParamsSelector :: Selector
readAttributeWattRatingWithParamsSelector = mkSelector "readAttributeWattRatingWithParams:"

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

