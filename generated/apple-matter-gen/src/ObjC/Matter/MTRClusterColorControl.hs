{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Color Control    Attributes and commands for controlling the color properties of a color-capable light.
--
-- Generated bindings for @MTRClusterColorControl@.
module ObjC.Matter.MTRClusterColorControl
  ( MTRClusterColorControl
  , IsMTRClusterColorControl(..)
  , moveToHueWithParams_expectedValues_expectedValueInterval_completion
  , moveHueWithParams_expectedValues_expectedValueInterval_completion
  , stepHueWithParams_expectedValues_expectedValueInterval_completion
  , moveToSaturationWithParams_expectedValues_expectedValueInterval_completion
  , moveSaturationWithParams_expectedValues_expectedValueInterval_completion
  , stepSaturationWithParams_expectedValues_expectedValueInterval_completion
  , moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion
  , moveToColorWithParams_expectedValues_expectedValueInterval_completion
  , moveColorWithParams_expectedValues_expectedValueInterval_completion
  , stepColorWithParams_expectedValues_expectedValueInterval_completion
  , moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completion
  , enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completion
  , enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completion
  , enhancedStepHueWithParams_expectedValues_expectedValueInterval_completion
  , enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion
  , colorLoopSetWithParams_expectedValues_expectedValueInterval_completion
  , stopMoveStepWithParams_expectedValues_expectedValueInterval_completion
  , moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completion
  , stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentHueWithParams
  , readAttributeCurrentSaturationWithParams
  , readAttributeRemainingTimeWithParams
  , readAttributeCurrentXWithParams
  , readAttributeCurrentYWithParams
  , readAttributeDriftCompensationWithParams
  , readAttributeCompensationTextWithParams
  , readAttributeColorTemperatureMiredsWithParams
  , readAttributeColorModeWithParams
  , readAttributeOptionsWithParams
  , writeAttributeOptionsWithValue_expectedValueInterval
  , writeAttributeOptionsWithValue_expectedValueInterval_params
  , readAttributeNumberOfPrimariesWithParams
  , readAttributePrimary1XWithParams
  , readAttributePrimary1YWithParams
  , readAttributePrimary1IntensityWithParams
  , readAttributePrimary2XWithParams
  , readAttributePrimary2YWithParams
  , readAttributePrimary2IntensityWithParams
  , readAttributePrimary3XWithParams
  , readAttributePrimary3YWithParams
  , readAttributePrimary3IntensityWithParams
  , readAttributePrimary4XWithParams
  , readAttributePrimary4YWithParams
  , readAttributePrimary4IntensityWithParams
  , readAttributePrimary5XWithParams
  , readAttributePrimary5YWithParams
  , readAttributePrimary5IntensityWithParams
  , readAttributePrimary6XWithParams
  , readAttributePrimary6YWithParams
  , readAttributePrimary6IntensityWithParams
  , readAttributeWhitePointXWithParams
  , readAttributeWhitePointYWithParams
  , readAttributeColorPointRXWithParams
  , readAttributeColorPointRYWithParams
  , readAttributeColorPointRIntensityWithParams
  , readAttributeColorPointGXWithParams
  , readAttributeColorPointGYWithParams
  , readAttributeColorPointGIntensityWithParams
  , readAttributeColorPointBXWithParams
  , readAttributeColorPointBYWithParams
  , readAttributeColorPointBIntensityWithParams
  , readAttributeEnhancedCurrentHueWithParams
  , readAttributeEnhancedColorModeWithParams
  , readAttributeColorLoopActiveWithParams
  , readAttributeColorLoopDirectionWithParams
  , readAttributeColorLoopTimeWithParams
  , readAttributeColorLoopStartEnhancedHueWithParams
  , readAttributeColorLoopStoredEnhancedHueWithParams
  , readAttributeColorCapabilitiesWithParams
  , readAttributeColorTempPhysicalMinMiredsWithParams
  , readAttributeColorTempPhysicalMaxMiredsWithParams
  , readAttributeCoupleColorTempToLevelMinMiredsWithParams
  , readAttributeStartUpColorTemperatureMiredsWithParams
  , writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval
  , writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , writeAttributeWhitePointXWithValue_expectedValueInterval
  , writeAttributeWhitePointXWithValue_expectedValueInterval_params
  , writeAttributeWhitePointYWithValue_expectedValueInterval
  , writeAttributeWhitePointYWithValue_expectedValueInterval_params
  , writeAttributeColorPointRXWithValue_expectedValueInterval
  , writeAttributeColorPointRXWithValue_expectedValueInterval_params
  , writeAttributeColorPointRYWithValue_expectedValueInterval
  , writeAttributeColorPointRYWithValue_expectedValueInterval_params
  , writeAttributeColorPointRIntensityWithValue_expectedValueInterval
  , writeAttributeColorPointRIntensityWithValue_expectedValueInterval_params
  , writeAttributeColorPointGXWithValue_expectedValueInterval
  , writeAttributeColorPointGXWithValue_expectedValueInterval_params
  , writeAttributeColorPointGYWithValue_expectedValueInterval
  , writeAttributeColorPointGYWithValue_expectedValueInterval_params
  , writeAttributeColorPointGIntensityWithValue_expectedValueInterval
  , writeAttributeColorPointGIntensityWithValue_expectedValueInterval_params
  , writeAttributeColorPointBXWithValue_expectedValueInterval
  , writeAttributeColorPointBXWithValue_expectedValueInterval_params
  , writeAttributeColorPointBYWithValue_expectedValueInterval
  , writeAttributeColorPointBYWithValue_expectedValueInterval_params
  , writeAttributeColorPointBIntensityWithValue_expectedValueInterval
  , writeAttributeColorPointBIntensityWithValue_expectedValueInterval_params
  , initWithDevice_endpoint_queue
  , moveToHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToColorWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveColorWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepColorWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler
  , enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandler
  , enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler
  , colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , moveToHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToSaturationWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveSaturationWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepSaturationWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToColorWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveColorWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepColorWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector
  , enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionSelector
  , enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector
  , colorLoopSetWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopMoveStepWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentHueWithParamsSelector
  , readAttributeCurrentSaturationWithParamsSelector
  , readAttributeRemainingTimeWithParamsSelector
  , readAttributeCurrentXWithParamsSelector
  , readAttributeCurrentYWithParamsSelector
  , readAttributeDriftCompensationWithParamsSelector
  , readAttributeCompensationTextWithParamsSelector
  , readAttributeColorTemperatureMiredsWithParamsSelector
  , readAttributeColorModeWithParamsSelector
  , readAttributeOptionsWithParamsSelector
  , writeAttributeOptionsWithValue_expectedValueIntervalSelector
  , writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector
  , readAttributeNumberOfPrimariesWithParamsSelector
  , readAttributePrimary1XWithParamsSelector
  , readAttributePrimary1YWithParamsSelector
  , readAttributePrimary1IntensityWithParamsSelector
  , readAttributePrimary2XWithParamsSelector
  , readAttributePrimary2YWithParamsSelector
  , readAttributePrimary2IntensityWithParamsSelector
  , readAttributePrimary3XWithParamsSelector
  , readAttributePrimary3YWithParamsSelector
  , readAttributePrimary3IntensityWithParamsSelector
  , readAttributePrimary4XWithParamsSelector
  , readAttributePrimary4YWithParamsSelector
  , readAttributePrimary4IntensityWithParamsSelector
  , readAttributePrimary5XWithParamsSelector
  , readAttributePrimary5YWithParamsSelector
  , readAttributePrimary5IntensityWithParamsSelector
  , readAttributePrimary6XWithParamsSelector
  , readAttributePrimary6YWithParamsSelector
  , readAttributePrimary6IntensityWithParamsSelector
  , readAttributeWhitePointXWithParamsSelector
  , readAttributeWhitePointYWithParamsSelector
  , readAttributeColorPointRXWithParamsSelector
  , readAttributeColorPointRYWithParamsSelector
  , readAttributeColorPointRIntensityWithParamsSelector
  , readAttributeColorPointGXWithParamsSelector
  , readAttributeColorPointGYWithParamsSelector
  , readAttributeColorPointGIntensityWithParamsSelector
  , readAttributeColorPointBXWithParamsSelector
  , readAttributeColorPointBYWithParamsSelector
  , readAttributeColorPointBIntensityWithParamsSelector
  , readAttributeEnhancedCurrentHueWithParamsSelector
  , readAttributeEnhancedColorModeWithParamsSelector
  , readAttributeColorLoopActiveWithParamsSelector
  , readAttributeColorLoopDirectionWithParamsSelector
  , readAttributeColorLoopTimeWithParamsSelector
  , readAttributeColorLoopStartEnhancedHueWithParamsSelector
  , readAttributeColorLoopStoredEnhancedHueWithParamsSelector
  , readAttributeColorCapabilitiesWithParamsSelector
  , readAttributeColorTempPhysicalMinMiredsWithParamsSelector
  , readAttributeColorTempPhysicalMaxMiredsWithParamsSelector
  , readAttributeCoupleColorTempToLevelMinMiredsWithParamsSelector
  , readAttributeStartUpColorTemperatureMiredsWithParamsSelector
  , writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , writeAttributeWhitePointXWithValue_expectedValueIntervalSelector
  , writeAttributeWhitePointXWithValue_expectedValueInterval_paramsSelector
  , writeAttributeWhitePointYWithValue_expectedValueIntervalSelector
  , writeAttributeWhitePointYWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointRXWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointRXWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointRYWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointRYWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointRIntensityWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointRIntensityWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointGXWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointGXWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointGYWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointGYWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointGIntensityWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointGIntensityWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointBXWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointBXWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointBYWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointBYWithValue_expectedValueInterval_paramsSelector
  , writeAttributeColorPointBIntensityWithValue_expectedValueIntervalSelector
  , writeAttributeColorPointBIntensityWithValue_expectedValueInterval_paramsSelector
  , initWithDevice_endpoint_queueSelector
  , moveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- moveToHueWithParams:expectedValues:expectedValueInterval:completion:@
moveToHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveHueWithParams:expectedValues:expectedValueInterval:completion:@
moveHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepHueWithParams:expectedValues:expectedValueInterval:completion:@
stepHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveToSaturationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToSaturationWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToSaturationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveSaturationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveSaturationWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveSaturationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepSaturationWithParams:expectedValues:expectedValueInterval:completion:@
stepSaturationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepSaturationWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepSaturationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToHueAndSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToColorWithParams:expectedValues:expectedValueInterval:completion:@
moveToColorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToColorWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToColorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveColorWithParams:expectedValues:expectedValueInterval:completion:@
moveColorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveColorWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveColorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepColorWithParams:expectedValues:expectedValueInterval:completion:@
stepColorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepColorWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepColorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveToHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enhancedStepHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedStepHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedStepHueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- colorLoopSetWithParams:expectedValues:expectedValueInterval:completion:@
colorLoopSetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterColorLoopSetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
colorLoopSetWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "colorLoopSetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopMoveStepWithParams:expectedValues:expectedValueInterval:completion:@
stopMoveStepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStopMoveStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMoveStepWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stopMoveStepWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completion mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentHueWithParams:@
readAttributeCurrentHueWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCurrentHueWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCurrentHueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentSaturationWithParams:@
readAttributeCurrentSaturationWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCurrentSaturationWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCurrentSaturationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeRemainingTimeWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeRemainingTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentXWithParams:@
readAttributeCurrentXWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCurrentXWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCurrentXWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentYWithParams:@
readAttributeCurrentYWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCurrentYWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCurrentYWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDriftCompensationWithParams:@
readAttributeDriftCompensationWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeDriftCompensationWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeDriftCompensationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCompensationTextWithParams:@
readAttributeCompensationTextWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCompensationTextWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCompensationTextWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorTemperatureMiredsWithParams:@
readAttributeColorTemperatureMiredsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorTemperatureMiredsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorTemperatureMiredsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorModeWithParams:@
readAttributeColorModeWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorModeWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOptionsWithParams:@
readAttributeOptionsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeOptionsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeOptionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeNumberOfPrimariesWithParams:@
readAttributeNumberOfPrimariesWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeNumberOfPrimariesWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeNumberOfPrimariesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary1XWithParams:@
readAttributePrimary1XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary1XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary1XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary1YWithParams:@
readAttributePrimary1YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary1YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary1YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary1IntensityWithParams:@
readAttributePrimary1IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary1IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary1IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary2XWithParams:@
readAttributePrimary2XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary2XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary2XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary2YWithParams:@
readAttributePrimary2YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary2YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary2YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary2IntensityWithParams:@
readAttributePrimary2IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary2IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary2IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary3XWithParams:@
readAttributePrimary3XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary3XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary3XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary3YWithParams:@
readAttributePrimary3YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary3YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary3YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary3IntensityWithParams:@
readAttributePrimary3IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary3IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary3IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary4XWithParams:@
readAttributePrimary4XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary4XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary4XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary4YWithParams:@
readAttributePrimary4YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary4YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary4YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary4IntensityWithParams:@
readAttributePrimary4IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary4IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary4IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary5XWithParams:@
readAttributePrimary5XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary5XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary5XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary5YWithParams:@
readAttributePrimary5YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary5YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary5YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary5IntensityWithParams:@
readAttributePrimary5IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary5IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary5IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary6XWithParams:@
readAttributePrimary6XWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary6XWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary6XWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary6YWithParams:@
readAttributePrimary6YWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary6YWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary6YWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePrimary6IntensityWithParams:@
readAttributePrimary6IntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributePrimary6IntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributePrimary6IntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWhitePointXWithParams:@
readAttributeWhitePointXWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeWhitePointXWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeWhitePointXWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWhitePointYWithParams:@
readAttributeWhitePointYWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeWhitePointYWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeWhitePointYWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointRXWithParams:@
readAttributeColorPointRXWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointRXWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointRXWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointRYWithParams:@
readAttributeColorPointRYWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointRYWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointRYWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointRIntensityWithParams:@
readAttributeColorPointRIntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointRIntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointRIntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointGXWithParams:@
readAttributeColorPointGXWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointGXWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointGXWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointGYWithParams:@
readAttributeColorPointGYWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointGYWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointGYWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointGIntensityWithParams:@
readAttributeColorPointGIntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointGIntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointGIntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointBXWithParams:@
readAttributeColorPointBXWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointBXWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointBXWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointBYWithParams:@
readAttributeColorPointBYWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointBYWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointBYWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorPointBIntensityWithParams:@
readAttributeColorPointBIntensityWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorPointBIntensityWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorPointBIntensityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEnhancedCurrentHueWithParams:@
readAttributeEnhancedCurrentHueWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeEnhancedCurrentHueWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeEnhancedCurrentHueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEnhancedColorModeWithParams:@
readAttributeEnhancedColorModeWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeEnhancedColorModeWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeEnhancedColorModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorLoopActiveWithParams:@
readAttributeColorLoopActiveWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorLoopActiveWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorLoopActiveWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorLoopDirectionWithParams:@
readAttributeColorLoopDirectionWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorLoopDirectionWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorLoopDirectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorLoopTimeWithParams:@
readAttributeColorLoopTimeWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorLoopTimeWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorLoopTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorLoopStartEnhancedHueWithParams:@
readAttributeColorLoopStartEnhancedHueWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorLoopStartEnhancedHueWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorLoopStartEnhancedHueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorLoopStoredEnhancedHueWithParams:@
readAttributeColorLoopStoredEnhancedHueWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorLoopStoredEnhancedHueWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorLoopStoredEnhancedHueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorCapabilitiesWithParams:@
readAttributeColorCapabilitiesWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorCapabilitiesWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorCapabilitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorTempPhysicalMinMiredsWithParams:@
readAttributeColorTempPhysicalMinMiredsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorTempPhysicalMinMiredsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorTempPhysicalMinMiredsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeColorTempPhysicalMaxMiredsWithParams:@
readAttributeColorTempPhysicalMaxMiredsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeColorTempPhysicalMaxMiredsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeColorTempPhysicalMaxMiredsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCoupleColorTempToLevelMinMiredsWithParams:@
readAttributeCoupleColorTempToLevelMinMiredsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeCoupleColorTempToLevelMinMiredsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeCoupleColorTempToLevelMinMiredsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStartUpColorTemperatureMiredsWithParams:@
readAttributeStartUpColorTemperatureMiredsWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeStartUpColorTemperatureMiredsWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeStartUpColorTemperatureMiredsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:@
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:params:@
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRReadParams params) => mtrClusterColorControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterColorControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterColorControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterColorControl mtrClusterColorControl => mtrClusterColorControl -> IO (Id MTRClusterColorControl)
init_ mtrClusterColorControl  =
    sendMsg mtrClusterColorControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterColorControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterColorControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- writeAttributeWhitePointXWithValue:expectedValueInterval:@
writeAttributeWhitePointXWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeWhitePointXWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeWhitePointXWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeWhitePointXWithValue:expectedValueInterval:params:@
writeAttributeWhitePointXWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeWhitePointXWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeWhitePointXWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeWhitePointYWithValue:expectedValueInterval:@
writeAttributeWhitePointYWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeWhitePointYWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeWhitePointYWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeWhitePointYWithValue:expectedValueInterval:params:@
writeAttributeWhitePointYWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeWhitePointYWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeWhitePointYWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointRXWithValue:expectedValueInterval:@
writeAttributeColorPointRXWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointRXWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRXWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointRXWithValue:expectedValueInterval:params:@
writeAttributeColorPointRXWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointRXWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRXWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointRYWithValue:expectedValueInterval:@
writeAttributeColorPointRYWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointRYWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRYWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointRYWithValue:expectedValueInterval:params:@
writeAttributeColorPointRYWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointRYWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRYWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointRIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointRIntensityWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointRIntensityWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRIntensityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointRIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointRIntensityWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointRIntensityWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointRIntensityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointGXWithValue:expectedValueInterval:@
writeAttributeColorPointGXWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointGXWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGXWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointGXWithValue:expectedValueInterval:params:@
writeAttributeColorPointGXWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointGXWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGXWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointGYWithValue:expectedValueInterval:@
writeAttributeColorPointGYWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointGYWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGYWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointGYWithValue:expectedValueInterval:params:@
writeAttributeColorPointGYWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointGYWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGYWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointGIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointGIntensityWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointGIntensityWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGIntensityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointGIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointGIntensityWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointGIntensityWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointGIntensityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointBXWithValue:expectedValueInterval:@
writeAttributeColorPointBXWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointBXWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBXWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointBXWithValue:expectedValueInterval:params:@
writeAttributeColorPointBXWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointBXWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBXWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointBYWithValue:expectedValueInterval:@
writeAttributeColorPointBYWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointBYWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBYWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointBYWithValue:expectedValueInterval:params:@
writeAttributeColorPointBYWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointBYWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBYWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- writeAttributeColorPointBIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointBIntensityWithValue_expectedValueInterval :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeColorPointBIntensityWithValue_expectedValueInterval mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBIntensityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeColorPointBIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointBIntensityWithValue_expectedValueInterval_params :: (IsMTRClusterColorControl mtrClusterColorControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterColorControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeColorPointBIntensityWithValue_expectedValueInterval_params mtrClusterColorControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterColorControl (mkSelector "writeAttributeColorPointBIntensityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRDevice device, IsNSObject queue) => mtrClusterColorControl -> device -> CUShort -> queue -> IO (Id MTRClusterColorControl)
initWithDevice_endpoint_queue mtrClusterColorControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterColorControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- moveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToHueAndSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToColorWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToColorWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToColorWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveColorWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveColorWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveColorWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepColorWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepColorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepColorWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepColorWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveToColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveToHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enhancedStepHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedStepHueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedStepHueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- colorLoopSetWithParams:expectedValues:expectedValueInterval:completionHandler:@
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterColorLoopSetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "colorLoopSetWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopMoveStepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStopMoveStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stopMoveStepWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterMoveColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRColorControlClusterStepColorTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterColorControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterColorControl  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterColorControl (mkSelector "stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterColorControl mtrClusterColorControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterColorControl -> device -> endpointID -> queue -> IO (Id MTRClusterColorControl)
initWithDevice_endpointID_queue mtrClusterColorControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterColorControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveToHueWithParams:expectedValues:expectedValueInterval:completion:@
moveToHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveHueWithParams:expectedValues:expectedValueInterval:completion:@
moveHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepHueWithParams:expectedValues:expectedValueInterval:completion:@
stepHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToSaturationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveSaturationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveSaturationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveSaturationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepSaturationWithParams:expectedValues:expectedValueInterval:completion:@
stepSaturationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepSaturationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepSaturationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:@
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToColorWithParams:expectedValues:expectedValueInterval:completion:@
moveToColorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToColorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToColorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveColorWithParams:expectedValues:expectedValueInterval:completion:@
moveColorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveColorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveColorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepColorWithParams:expectedValues:expectedValueInterval:completion:@
stepColorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepColorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepColorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enhancedStepHueWithParams:expectedValues:expectedValueInterval:completion:@
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enhancedStepHueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:@
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @colorLoopSetWithParams:expectedValues:expectedValueInterval:completion:@
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "colorLoopSetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopMoveStepWithParams:expectedValues:expectedValueInterval:completion:@
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopMoveStepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentHueWithParams:@
readAttributeCurrentHueWithParamsSelector :: Selector
readAttributeCurrentHueWithParamsSelector = mkSelector "readAttributeCurrentHueWithParams:"

-- | @Selector@ for @readAttributeCurrentSaturationWithParams:@
readAttributeCurrentSaturationWithParamsSelector :: Selector
readAttributeCurrentSaturationWithParamsSelector = mkSelector "readAttributeCurrentSaturationWithParams:"

-- | @Selector@ for @readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParamsSelector :: Selector
readAttributeRemainingTimeWithParamsSelector = mkSelector "readAttributeRemainingTimeWithParams:"

-- | @Selector@ for @readAttributeCurrentXWithParams:@
readAttributeCurrentXWithParamsSelector :: Selector
readAttributeCurrentXWithParamsSelector = mkSelector "readAttributeCurrentXWithParams:"

-- | @Selector@ for @readAttributeCurrentYWithParams:@
readAttributeCurrentYWithParamsSelector :: Selector
readAttributeCurrentYWithParamsSelector = mkSelector "readAttributeCurrentYWithParams:"

-- | @Selector@ for @readAttributeDriftCompensationWithParams:@
readAttributeDriftCompensationWithParamsSelector :: Selector
readAttributeDriftCompensationWithParamsSelector = mkSelector "readAttributeDriftCompensationWithParams:"

-- | @Selector@ for @readAttributeCompensationTextWithParams:@
readAttributeCompensationTextWithParamsSelector :: Selector
readAttributeCompensationTextWithParamsSelector = mkSelector "readAttributeCompensationTextWithParams:"

-- | @Selector@ for @readAttributeColorTemperatureMiredsWithParams:@
readAttributeColorTemperatureMiredsWithParamsSelector :: Selector
readAttributeColorTemperatureMiredsWithParamsSelector = mkSelector "readAttributeColorTemperatureMiredsWithParams:"

-- | @Selector@ for @readAttributeColorModeWithParams:@
readAttributeColorModeWithParamsSelector :: Selector
readAttributeColorModeWithParamsSelector = mkSelector "readAttributeColorModeWithParams:"

-- | @Selector@ for @readAttributeOptionsWithParams:@
readAttributeOptionsWithParamsSelector :: Selector
readAttributeOptionsWithParamsSelector = mkSelector "readAttributeOptionsWithParams:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOptionsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNumberOfPrimariesWithParams:@
readAttributeNumberOfPrimariesWithParamsSelector :: Selector
readAttributeNumberOfPrimariesWithParamsSelector = mkSelector "readAttributeNumberOfPrimariesWithParams:"

-- | @Selector@ for @readAttributePrimary1XWithParams:@
readAttributePrimary1XWithParamsSelector :: Selector
readAttributePrimary1XWithParamsSelector = mkSelector "readAttributePrimary1XWithParams:"

-- | @Selector@ for @readAttributePrimary1YWithParams:@
readAttributePrimary1YWithParamsSelector :: Selector
readAttributePrimary1YWithParamsSelector = mkSelector "readAttributePrimary1YWithParams:"

-- | @Selector@ for @readAttributePrimary1IntensityWithParams:@
readAttributePrimary1IntensityWithParamsSelector :: Selector
readAttributePrimary1IntensityWithParamsSelector = mkSelector "readAttributePrimary1IntensityWithParams:"

-- | @Selector@ for @readAttributePrimary2XWithParams:@
readAttributePrimary2XWithParamsSelector :: Selector
readAttributePrimary2XWithParamsSelector = mkSelector "readAttributePrimary2XWithParams:"

-- | @Selector@ for @readAttributePrimary2YWithParams:@
readAttributePrimary2YWithParamsSelector :: Selector
readAttributePrimary2YWithParamsSelector = mkSelector "readAttributePrimary2YWithParams:"

-- | @Selector@ for @readAttributePrimary2IntensityWithParams:@
readAttributePrimary2IntensityWithParamsSelector :: Selector
readAttributePrimary2IntensityWithParamsSelector = mkSelector "readAttributePrimary2IntensityWithParams:"

-- | @Selector@ for @readAttributePrimary3XWithParams:@
readAttributePrimary3XWithParamsSelector :: Selector
readAttributePrimary3XWithParamsSelector = mkSelector "readAttributePrimary3XWithParams:"

-- | @Selector@ for @readAttributePrimary3YWithParams:@
readAttributePrimary3YWithParamsSelector :: Selector
readAttributePrimary3YWithParamsSelector = mkSelector "readAttributePrimary3YWithParams:"

-- | @Selector@ for @readAttributePrimary3IntensityWithParams:@
readAttributePrimary3IntensityWithParamsSelector :: Selector
readAttributePrimary3IntensityWithParamsSelector = mkSelector "readAttributePrimary3IntensityWithParams:"

-- | @Selector@ for @readAttributePrimary4XWithParams:@
readAttributePrimary4XWithParamsSelector :: Selector
readAttributePrimary4XWithParamsSelector = mkSelector "readAttributePrimary4XWithParams:"

-- | @Selector@ for @readAttributePrimary4YWithParams:@
readAttributePrimary4YWithParamsSelector :: Selector
readAttributePrimary4YWithParamsSelector = mkSelector "readAttributePrimary4YWithParams:"

-- | @Selector@ for @readAttributePrimary4IntensityWithParams:@
readAttributePrimary4IntensityWithParamsSelector :: Selector
readAttributePrimary4IntensityWithParamsSelector = mkSelector "readAttributePrimary4IntensityWithParams:"

-- | @Selector@ for @readAttributePrimary5XWithParams:@
readAttributePrimary5XWithParamsSelector :: Selector
readAttributePrimary5XWithParamsSelector = mkSelector "readAttributePrimary5XWithParams:"

-- | @Selector@ for @readAttributePrimary5YWithParams:@
readAttributePrimary5YWithParamsSelector :: Selector
readAttributePrimary5YWithParamsSelector = mkSelector "readAttributePrimary5YWithParams:"

-- | @Selector@ for @readAttributePrimary5IntensityWithParams:@
readAttributePrimary5IntensityWithParamsSelector :: Selector
readAttributePrimary5IntensityWithParamsSelector = mkSelector "readAttributePrimary5IntensityWithParams:"

-- | @Selector@ for @readAttributePrimary6XWithParams:@
readAttributePrimary6XWithParamsSelector :: Selector
readAttributePrimary6XWithParamsSelector = mkSelector "readAttributePrimary6XWithParams:"

-- | @Selector@ for @readAttributePrimary6YWithParams:@
readAttributePrimary6YWithParamsSelector :: Selector
readAttributePrimary6YWithParamsSelector = mkSelector "readAttributePrimary6YWithParams:"

-- | @Selector@ for @readAttributePrimary6IntensityWithParams:@
readAttributePrimary6IntensityWithParamsSelector :: Selector
readAttributePrimary6IntensityWithParamsSelector = mkSelector "readAttributePrimary6IntensityWithParams:"

-- | @Selector@ for @readAttributeWhitePointXWithParams:@
readAttributeWhitePointXWithParamsSelector :: Selector
readAttributeWhitePointXWithParamsSelector = mkSelector "readAttributeWhitePointXWithParams:"

-- | @Selector@ for @readAttributeWhitePointYWithParams:@
readAttributeWhitePointYWithParamsSelector :: Selector
readAttributeWhitePointYWithParamsSelector = mkSelector "readAttributeWhitePointYWithParams:"

-- | @Selector@ for @readAttributeColorPointRXWithParams:@
readAttributeColorPointRXWithParamsSelector :: Selector
readAttributeColorPointRXWithParamsSelector = mkSelector "readAttributeColorPointRXWithParams:"

-- | @Selector@ for @readAttributeColorPointRYWithParams:@
readAttributeColorPointRYWithParamsSelector :: Selector
readAttributeColorPointRYWithParamsSelector = mkSelector "readAttributeColorPointRYWithParams:"

-- | @Selector@ for @readAttributeColorPointRIntensityWithParams:@
readAttributeColorPointRIntensityWithParamsSelector :: Selector
readAttributeColorPointRIntensityWithParamsSelector = mkSelector "readAttributeColorPointRIntensityWithParams:"

-- | @Selector@ for @readAttributeColorPointGXWithParams:@
readAttributeColorPointGXWithParamsSelector :: Selector
readAttributeColorPointGXWithParamsSelector = mkSelector "readAttributeColorPointGXWithParams:"

-- | @Selector@ for @readAttributeColorPointGYWithParams:@
readAttributeColorPointGYWithParamsSelector :: Selector
readAttributeColorPointGYWithParamsSelector = mkSelector "readAttributeColorPointGYWithParams:"

-- | @Selector@ for @readAttributeColorPointGIntensityWithParams:@
readAttributeColorPointGIntensityWithParamsSelector :: Selector
readAttributeColorPointGIntensityWithParamsSelector = mkSelector "readAttributeColorPointGIntensityWithParams:"

-- | @Selector@ for @readAttributeColorPointBXWithParams:@
readAttributeColorPointBXWithParamsSelector :: Selector
readAttributeColorPointBXWithParamsSelector = mkSelector "readAttributeColorPointBXWithParams:"

-- | @Selector@ for @readAttributeColorPointBYWithParams:@
readAttributeColorPointBYWithParamsSelector :: Selector
readAttributeColorPointBYWithParamsSelector = mkSelector "readAttributeColorPointBYWithParams:"

-- | @Selector@ for @readAttributeColorPointBIntensityWithParams:@
readAttributeColorPointBIntensityWithParamsSelector :: Selector
readAttributeColorPointBIntensityWithParamsSelector = mkSelector "readAttributeColorPointBIntensityWithParams:"

-- | @Selector@ for @readAttributeEnhancedCurrentHueWithParams:@
readAttributeEnhancedCurrentHueWithParamsSelector :: Selector
readAttributeEnhancedCurrentHueWithParamsSelector = mkSelector "readAttributeEnhancedCurrentHueWithParams:"

-- | @Selector@ for @readAttributeEnhancedColorModeWithParams:@
readAttributeEnhancedColorModeWithParamsSelector :: Selector
readAttributeEnhancedColorModeWithParamsSelector = mkSelector "readAttributeEnhancedColorModeWithParams:"

-- | @Selector@ for @readAttributeColorLoopActiveWithParams:@
readAttributeColorLoopActiveWithParamsSelector :: Selector
readAttributeColorLoopActiveWithParamsSelector = mkSelector "readAttributeColorLoopActiveWithParams:"

-- | @Selector@ for @readAttributeColorLoopDirectionWithParams:@
readAttributeColorLoopDirectionWithParamsSelector :: Selector
readAttributeColorLoopDirectionWithParamsSelector = mkSelector "readAttributeColorLoopDirectionWithParams:"

-- | @Selector@ for @readAttributeColorLoopTimeWithParams:@
readAttributeColorLoopTimeWithParamsSelector :: Selector
readAttributeColorLoopTimeWithParamsSelector = mkSelector "readAttributeColorLoopTimeWithParams:"

-- | @Selector@ for @readAttributeColorLoopStartEnhancedHueWithParams:@
readAttributeColorLoopStartEnhancedHueWithParamsSelector :: Selector
readAttributeColorLoopStartEnhancedHueWithParamsSelector = mkSelector "readAttributeColorLoopStartEnhancedHueWithParams:"

-- | @Selector@ for @readAttributeColorLoopStoredEnhancedHueWithParams:@
readAttributeColorLoopStoredEnhancedHueWithParamsSelector :: Selector
readAttributeColorLoopStoredEnhancedHueWithParamsSelector = mkSelector "readAttributeColorLoopStoredEnhancedHueWithParams:"

-- | @Selector@ for @readAttributeColorCapabilitiesWithParams:@
readAttributeColorCapabilitiesWithParamsSelector :: Selector
readAttributeColorCapabilitiesWithParamsSelector = mkSelector "readAttributeColorCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeColorTempPhysicalMinMiredsWithParams:@
readAttributeColorTempPhysicalMinMiredsWithParamsSelector :: Selector
readAttributeColorTempPhysicalMinMiredsWithParamsSelector = mkSelector "readAttributeColorTempPhysicalMinMiredsWithParams:"

-- | @Selector@ for @readAttributeColorTempPhysicalMaxMiredsWithParams:@
readAttributeColorTempPhysicalMaxMiredsWithParamsSelector :: Selector
readAttributeColorTempPhysicalMaxMiredsWithParamsSelector = mkSelector "readAttributeColorTempPhysicalMaxMiredsWithParams:"

-- | @Selector@ for @readAttributeCoupleColorTempToLevelMinMiredsWithParams:@
readAttributeCoupleColorTempToLevelMinMiredsWithParamsSelector :: Selector
readAttributeCoupleColorTempToLevelMinMiredsWithParamsSelector = mkSelector "readAttributeCoupleColorTempToLevelMinMiredsWithParams:"

-- | @Selector@ for @readAttributeStartUpColorTemperatureMiredsWithParams:@
readAttributeStartUpColorTemperatureMiredsWithParamsSelector :: Selector
readAttributeStartUpColorTemperatureMiredsWithParamsSelector = mkSelector "readAttributeStartUpColorTemperatureMiredsWithParams:"

-- | @Selector@ for @writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:@
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:params:@
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStartUpColorTemperatureMiredsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpColorTemperatureMiredsWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @writeAttributeWhitePointXWithValue:expectedValueInterval:@
writeAttributeWhitePointXWithValue_expectedValueIntervalSelector :: Selector
writeAttributeWhitePointXWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeWhitePointXWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeWhitePointXWithValue:expectedValueInterval:params:@
writeAttributeWhitePointXWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeWhitePointXWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeWhitePointXWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeWhitePointYWithValue:expectedValueInterval:@
writeAttributeWhitePointYWithValue_expectedValueIntervalSelector :: Selector
writeAttributeWhitePointYWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeWhitePointYWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeWhitePointYWithValue:expectedValueInterval:params:@
writeAttributeWhitePointYWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeWhitePointYWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeWhitePointYWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointRXWithValue:expectedValueInterval:@
writeAttributeColorPointRXWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointRXWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointRXWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointRXWithValue:expectedValueInterval:params:@
writeAttributeColorPointRXWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointRXWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointRXWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointRYWithValue:expectedValueInterval:@
writeAttributeColorPointRYWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointRYWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointRYWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointRYWithValue:expectedValueInterval:params:@
writeAttributeColorPointRYWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointRYWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointRYWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointRIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointRIntensityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointRIntensityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointRIntensityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointRIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointRIntensityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointRIntensityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointRIntensityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointGXWithValue:expectedValueInterval:@
writeAttributeColorPointGXWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointGXWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointGXWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointGXWithValue:expectedValueInterval:params:@
writeAttributeColorPointGXWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointGXWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointGXWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointGYWithValue:expectedValueInterval:@
writeAttributeColorPointGYWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointGYWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointGYWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointGYWithValue:expectedValueInterval:params:@
writeAttributeColorPointGYWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointGYWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointGYWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointGIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointGIntensityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointGIntensityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointGIntensityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointGIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointGIntensityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointGIntensityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointGIntensityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointBXWithValue:expectedValueInterval:@
writeAttributeColorPointBXWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointBXWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointBXWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointBXWithValue:expectedValueInterval:params:@
writeAttributeColorPointBXWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointBXWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointBXWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointBYWithValue:expectedValueInterval:@
writeAttributeColorPointBYWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointBYWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointBYWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointBYWithValue:expectedValueInterval:params:@
writeAttributeColorPointBYWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointBYWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointBYWithValue:expectedValueInterval:params:"

-- | @Selector@ for @writeAttributeColorPointBIntensityWithValue:expectedValueInterval:@
writeAttributeColorPointBIntensityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeColorPointBIntensityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeColorPointBIntensityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeColorPointBIntensityWithValue:expectedValueInterval:params:@
writeAttributeColorPointBIntensityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeColorPointBIntensityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeColorPointBIntensityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @moveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToColorWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveColorWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepColorWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepColorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepColorWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveToColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enhancedMoveToHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enhancedMoveToHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enhancedMoveHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enhancedMoveHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enhancedStepHueWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enhancedStepHueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enhancedStepHueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:@
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enhancedMoveToHueAndSaturationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enhancedMoveToHueAndSaturationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @colorLoopSetWithParams:expectedValues:expectedValueInterval:completionHandler:@
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
colorLoopSetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "colorLoopSetWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopMoveStepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopMoveStepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopMoveStepWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
moveColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stepColorTemperatureWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepColorTemperatureWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

