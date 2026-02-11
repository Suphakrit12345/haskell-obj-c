{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Window Covering    Provides an interface for controlling and adjusting automatic window coverings.
--
-- Generated bindings for @MTRClusterWindowCovering@.
module ObjC.Matter.MTRClusterWindowCovering
  ( MTRClusterWindowCovering
  , IsMTRClusterWindowCovering(..)
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completion
  , upOrOpenWithExpectedValues_expectedValueInterval_completion
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completion
  , downOrCloseWithExpectedValues_expectedValueInterval_completion
  , stopMotionWithParams_expectedValues_expectedValueInterval_completion
  , stopMotionWithExpectedValues_expectedValueInterval_completion
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completion
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completion
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTypeWithParams
  , readAttributePhysicalClosedLimitLiftWithParams
  , readAttributePhysicalClosedLimitTiltWithParams
  , readAttributeCurrentPositionLiftWithParams
  , readAttributeCurrentPositionTiltWithParams
  , readAttributeNumberOfActuationsLiftWithParams
  , readAttributeNumberOfActuationsTiltWithParams
  , readAttributeConfigStatusWithParams
  , readAttributeCurrentPositionLiftPercentageWithParams
  , readAttributeCurrentPositionTiltPercentageWithParams
  , readAttributeOperationalStatusWithParams
  , readAttributeTargetPositionLiftPercent100thsWithParams
  , readAttributeTargetPositionTiltPercent100thsWithParams
  , readAttributeEndProductTypeWithParams
  , readAttributeCurrentPositionLiftPercent100thsWithParams
  , readAttributeCurrentPositionTiltPercent100thsWithParams
  , readAttributeInstalledOpenLimitLiftWithParams
  , readAttributeInstalledClosedLimitLiftWithParams
  , readAttributeInstalledOpenLimitTiltWithParams
  , readAttributeInstalledClosedLimitTiltWithParams
  , readAttributeModeWithParams
  , writeAttributeModeWithValue_expectedValueInterval
  , writeAttributeModeWithValue_expectedValueInterval_params
  , readAttributeSafetyStatusWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler
  , upOrOpenWithExpectedValues_expectedValueInterval_completionHandler
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler
  , downOrCloseWithExpectedValues_expectedValueInterval_completionHandler
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopMotionWithExpectedValues_expectedValueInterval_completionHandler
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector
  , upOrOpenWithExpectedValues_expectedValueInterval_completionSelector
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector
  , downOrCloseWithExpectedValues_expectedValueInterval_completionSelector
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopMotionWithExpectedValues_expectedValueInterval_completionSelector
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeTypeWithParamsSelector
  , readAttributePhysicalClosedLimitLiftWithParamsSelector
  , readAttributePhysicalClosedLimitTiltWithParamsSelector
  , readAttributeCurrentPositionLiftWithParamsSelector
  , readAttributeCurrentPositionTiltWithParamsSelector
  , readAttributeNumberOfActuationsLiftWithParamsSelector
  , readAttributeNumberOfActuationsTiltWithParamsSelector
  , readAttributeConfigStatusWithParamsSelector
  , readAttributeCurrentPositionLiftPercentageWithParamsSelector
  , readAttributeCurrentPositionTiltPercentageWithParamsSelector
  , readAttributeOperationalStatusWithParamsSelector
  , readAttributeTargetPositionLiftPercent100thsWithParamsSelector
  , readAttributeTargetPositionTiltPercent100thsWithParamsSelector
  , readAttributeEndProductTypeWithParamsSelector
  , readAttributeCurrentPositionLiftPercent100thsWithParamsSelector
  , readAttributeCurrentPositionTiltPercent100thsWithParamsSelector
  , readAttributeInstalledOpenLimitLiftWithParamsSelector
  , readAttributeInstalledClosedLimitLiftWithParamsSelector
  , readAttributeInstalledOpenLimitTiltWithParamsSelector
  , readAttributeInstalledClosedLimitTiltWithParamsSelector
  , readAttributeModeWithParamsSelector
  , writeAttributeModeWithValue_expectedValueIntervalSelector
  , writeAttributeModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeSafetyStatusWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- upOrOpenWithParams:expectedValues:expectedValueInterval:completion:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterUpOrOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- upOrOpenWithExpectedValues:expectedValueInterval:completion:@
upOrOpenWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- downOrCloseWithParams:expectedValues:expectedValueInterval:completion:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterDownOrCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- downOrCloseWithExpectedValues:expectedValueInterval:completion:@
downOrCloseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopMotionWithParams:expectedValues:expectedValueInterval:completion:@
stopMotionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterStopMotionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopMotionWithExpectedValues:expectedValueInterval:completion:@
stopMotionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTypeWithParams:@
readAttributeTypeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTypeWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePhysicalClosedLimitLiftWithParams:@
readAttributePhysicalClosedLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributePhysicalClosedLimitLiftWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributePhysicalClosedLimitLiftWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePhysicalClosedLimitTiltWithParams:@
readAttributePhysicalClosedLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributePhysicalClosedLimitTiltWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributePhysicalClosedLimitTiltWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionLiftWithParams:@
readAttributeCurrentPositionLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionLiftWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionTiltWithParams:@
readAttributeCurrentPositionTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionTiltWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfActuationsLiftWithParams:@
readAttributeNumberOfActuationsLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeNumberOfActuationsLiftWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeNumberOfActuationsLiftWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfActuationsTiltWithParams:@
readAttributeNumberOfActuationsTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeNumberOfActuationsTiltWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeNumberOfActuationsTiltWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeConfigStatusWithParams:@
readAttributeConfigStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeConfigStatusWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeConfigStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionLiftPercentageWithParams:@
readAttributeCurrentPositionLiftPercentageWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftPercentageWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionLiftPercentageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionTiltPercentageWithParams:@
readAttributeCurrentPositionTiltPercentageWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltPercentageWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionTiltPercentageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalStatusWithParams:@
readAttributeOperationalStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeOperationalStatusWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeOperationalStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetPositionLiftPercent100thsWithParams:@
readAttributeTargetPositionLiftPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTargetPositionLiftPercent100thsWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeTargetPositionLiftPercent100thsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetPositionTiltPercent100thsWithParams:@
readAttributeTargetPositionTiltPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTargetPositionTiltPercent100thsWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeTargetPositionTiltPercent100thsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndProductTypeWithParams:@
readAttributeEndProductTypeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeEndProductTypeWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeEndProductTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionLiftPercent100thsWithParams:@
readAttributeCurrentPositionLiftPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftPercent100thsWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionLiftPercent100thsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionTiltPercent100thsWithParams:@
readAttributeCurrentPositionTiltPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltPercent100thsWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeCurrentPositionTiltPercent100thsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstalledOpenLimitLiftWithParams:@
readAttributeInstalledOpenLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledOpenLimitLiftWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeInstalledOpenLimitLiftWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstalledClosedLimitLiftWithParams:@
readAttributeInstalledClosedLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledClosedLimitLiftWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeInstalledClosedLimitLiftWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstalledOpenLimitTiltWithParams:@
readAttributeInstalledOpenLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledOpenLimitTiltWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeInstalledOpenLimitTiltWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstalledClosedLimitTiltWithParams:@
readAttributeInstalledClosedLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledClosedLimitTiltWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeInstalledClosedLimitTiltWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeModeWithParams:@
readAttributeModeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeModeWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeModeWithValue:expectedValueInterval:@
writeAttributeModeWithValue_expectedValueInterval :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeModeWithValue_expectedValueInterval mtrClusterWindowCovering  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "writeAttributeModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeModeWithValue:expectedValueInterval:params:@
writeAttributeModeWithValue_expectedValueInterval_params :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterWindowCovering -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeModeWithValue_expectedValueInterval_params mtrClusterWindowCovering  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterWindowCovering (mkSelector "writeAttributeModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSafetyStatusWithParams:@
readAttributeSafetyStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeSafetyStatusWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeSafetyStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWindowCovering  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterWindowCovering (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterWindowCovering mtrClusterWindowCovering => mtrClusterWindowCovering -> IO (Id MTRClusterWindowCovering)
init_ mtrClusterWindowCovering  =
    sendMsg mtrClusterWindowCovering (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterWindowCovering)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWindowCovering"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRDevice device, IsNSObject queue) => mtrClusterWindowCovering -> device -> CUShort -> queue -> IO (Id MTRClusterWindowCovering)
initWithDevice_endpoint_queue mtrClusterWindowCovering  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterWindowCovering (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterUpOrOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterDownOrCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterStopMotionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopMotionWithExpectedValues:expectedValueInterval:completionHandler:@
stopMotionWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterWindowCovering (mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterWindowCovering (mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWindowCovering -> device -> endpointID -> queue -> IO (Id MTRClusterWindowCovering)
initWithDevice_endpointID_queue mtrClusterWindowCovering  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterWindowCovering (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upOrOpenWithParams:expectedValues:expectedValueInterval:completion:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @upOrOpenWithExpectedValues:expectedValueInterval:completion:@
upOrOpenWithExpectedValues_expectedValueInterval_completionSelector :: Selector
upOrOpenWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @downOrCloseWithParams:expectedValues:expectedValueInterval:completion:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @downOrCloseWithExpectedValues:expectedValueInterval:completion:@
downOrCloseWithExpectedValues_expectedValueInterval_completionSelector :: Selector
downOrCloseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopMotionWithParams:expectedValues:expectedValueInterval:completion:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopMotionWithExpectedValues:expectedValueInterval:completion:@
stopMotionWithExpectedValues_expectedValueInterval_completionSelector :: Selector
stopMotionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTypeWithParams:@
readAttributeTypeWithParamsSelector :: Selector
readAttributeTypeWithParamsSelector = mkSelector "readAttributeTypeWithParams:"

-- | @Selector@ for @readAttributePhysicalClosedLimitLiftWithParams:@
readAttributePhysicalClosedLimitLiftWithParamsSelector :: Selector
readAttributePhysicalClosedLimitLiftWithParamsSelector = mkSelector "readAttributePhysicalClosedLimitLiftWithParams:"

-- | @Selector@ for @readAttributePhysicalClosedLimitTiltWithParams:@
readAttributePhysicalClosedLimitTiltWithParamsSelector :: Selector
readAttributePhysicalClosedLimitTiltWithParamsSelector = mkSelector "readAttributePhysicalClosedLimitTiltWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftWithParams:@
readAttributeCurrentPositionLiftWithParamsSelector :: Selector
readAttributeCurrentPositionLiftWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltWithParams:@
readAttributeCurrentPositionTiltWithParamsSelector :: Selector
readAttributeCurrentPositionTiltWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltWithParams:"

-- | @Selector@ for @readAttributeNumberOfActuationsLiftWithParams:@
readAttributeNumberOfActuationsLiftWithParamsSelector :: Selector
readAttributeNumberOfActuationsLiftWithParamsSelector = mkSelector "readAttributeNumberOfActuationsLiftWithParams:"

-- | @Selector@ for @readAttributeNumberOfActuationsTiltWithParams:@
readAttributeNumberOfActuationsTiltWithParamsSelector :: Selector
readAttributeNumberOfActuationsTiltWithParamsSelector = mkSelector "readAttributeNumberOfActuationsTiltWithParams:"

-- | @Selector@ for @readAttributeConfigStatusWithParams:@
readAttributeConfigStatusWithParamsSelector :: Selector
readAttributeConfigStatusWithParamsSelector = mkSelector "readAttributeConfigStatusWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftPercentageWithParams:@
readAttributeCurrentPositionLiftPercentageWithParamsSelector :: Selector
readAttributeCurrentPositionLiftPercentageWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftPercentageWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltPercentageWithParams:@
readAttributeCurrentPositionTiltPercentageWithParamsSelector :: Selector
readAttributeCurrentPositionTiltPercentageWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltPercentageWithParams:"

-- | @Selector@ for @readAttributeOperationalStatusWithParams:@
readAttributeOperationalStatusWithParamsSelector :: Selector
readAttributeOperationalStatusWithParamsSelector = mkSelector "readAttributeOperationalStatusWithParams:"

-- | @Selector@ for @readAttributeTargetPositionLiftPercent100thsWithParams:@
readAttributeTargetPositionLiftPercent100thsWithParamsSelector :: Selector
readAttributeTargetPositionLiftPercent100thsWithParamsSelector = mkSelector "readAttributeTargetPositionLiftPercent100thsWithParams:"

-- | @Selector@ for @readAttributeTargetPositionTiltPercent100thsWithParams:@
readAttributeTargetPositionTiltPercent100thsWithParamsSelector :: Selector
readAttributeTargetPositionTiltPercent100thsWithParamsSelector = mkSelector "readAttributeTargetPositionTiltPercent100thsWithParams:"

-- | @Selector@ for @readAttributeEndProductTypeWithParams:@
readAttributeEndProductTypeWithParamsSelector :: Selector
readAttributeEndProductTypeWithParamsSelector = mkSelector "readAttributeEndProductTypeWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftPercent100thsWithParams:@
readAttributeCurrentPositionLiftPercent100thsWithParamsSelector :: Selector
readAttributeCurrentPositionLiftPercent100thsWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftPercent100thsWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltPercent100thsWithParams:@
readAttributeCurrentPositionTiltPercent100thsWithParamsSelector :: Selector
readAttributeCurrentPositionTiltPercent100thsWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltPercent100thsWithParams:"

-- | @Selector@ for @readAttributeInstalledOpenLimitLiftWithParams:@
readAttributeInstalledOpenLimitLiftWithParamsSelector :: Selector
readAttributeInstalledOpenLimitLiftWithParamsSelector = mkSelector "readAttributeInstalledOpenLimitLiftWithParams:"

-- | @Selector@ for @readAttributeInstalledClosedLimitLiftWithParams:@
readAttributeInstalledClosedLimitLiftWithParamsSelector :: Selector
readAttributeInstalledClosedLimitLiftWithParamsSelector = mkSelector "readAttributeInstalledClosedLimitLiftWithParams:"

-- | @Selector@ for @readAttributeInstalledOpenLimitTiltWithParams:@
readAttributeInstalledOpenLimitTiltWithParamsSelector :: Selector
readAttributeInstalledOpenLimitTiltWithParamsSelector = mkSelector "readAttributeInstalledOpenLimitTiltWithParams:"

-- | @Selector@ for @readAttributeInstalledClosedLimitTiltWithParams:@
readAttributeInstalledClosedLimitTiltWithParamsSelector :: Selector
readAttributeInstalledClosedLimitTiltWithParamsSelector = mkSelector "readAttributeInstalledClosedLimitTiltWithParams:"

-- | @Selector@ for @readAttributeModeWithParams:@
readAttributeModeWithParamsSelector :: Selector
readAttributeModeWithParamsSelector = mkSelector "readAttributeModeWithParams:"

-- | @Selector@ for @writeAttributeModeWithValue:expectedValueInterval:@
writeAttributeModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeModeWithValue:expectedValueInterval:params:@
writeAttributeModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSafetyStatusWithParams:@
readAttributeSafetyStatusWithParamsSelector :: Selector
readAttributeSafetyStatusWithParamsSelector = mkSelector "readAttributeSafetyStatusWithParams:"

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

-- | @Selector@ for @upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopMotionWithExpectedValues:expectedValueInterval:completionHandler:@
stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

