{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Source    This cluster is used to describe the configuration and capabilities of a physical power source that provides power to the Node.
--
-- Generated bindings for @MTRClusterPowerSource@.
module ObjC.Matter.MTRClusterPowerSource
  ( MTRClusterPowerSource
  , IsMTRClusterPowerSource(..)
  , readAttributeStatusWithParams
  , readAttributeOrderWithParams
  , readAttributeDescriptionWithParams
  , readAttributeWiredAssessedInputVoltageWithParams
  , readAttributeWiredAssessedInputFrequencyWithParams
  , readAttributeWiredCurrentTypeWithParams
  , readAttributeWiredAssessedCurrentWithParams
  , readAttributeWiredNominalVoltageWithParams
  , readAttributeWiredMaximumCurrentWithParams
  , readAttributeWiredPresentWithParams
  , readAttributeActiveWiredFaultsWithParams
  , readAttributeBatVoltageWithParams
  , readAttributeBatPercentRemainingWithParams
  , readAttributeBatTimeRemainingWithParams
  , readAttributeBatChargeLevelWithParams
  , readAttributeBatReplacementNeededWithParams
  , readAttributeBatReplaceabilityWithParams
  , readAttributeBatPresentWithParams
  , readAttributeActiveBatFaultsWithParams
  , readAttributeBatReplacementDescriptionWithParams
  , readAttributeBatCommonDesignationWithParams
  , readAttributeBatANSIDesignationWithParams
  , readAttributeBatIECDesignationWithParams
  , readAttributeBatApprovedChemistryWithParams
  , readAttributeBatCapacityWithParams
  , readAttributeBatQuantityWithParams
  , readAttributeBatChargeStateWithParams
  , readAttributeBatTimeToFullChargeWithParams
  , readAttributeBatFunctionalWhileChargingWithParams
  , readAttributeBatChargingCurrentWithParams
  , readAttributeActiveBatChargeFaultsWithParams
  , readAttributeEndpointListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeStatusWithParamsSelector
  , readAttributeOrderWithParamsSelector
  , readAttributeDescriptionWithParamsSelector
  , readAttributeWiredAssessedInputVoltageWithParamsSelector
  , readAttributeWiredAssessedInputFrequencyWithParamsSelector
  , readAttributeWiredCurrentTypeWithParamsSelector
  , readAttributeWiredAssessedCurrentWithParamsSelector
  , readAttributeWiredNominalVoltageWithParamsSelector
  , readAttributeWiredMaximumCurrentWithParamsSelector
  , readAttributeWiredPresentWithParamsSelector
  , readAttributeActiveWiredFaultsWithParamsSelector
  , readAttributeBatVoltageWithParamsSelector
  , readAttributeBatPercentRemainingWithParamsSelector
  , readAttributeBatTimeRemainingWithParamsSelector
  , readAttributeBatChargeLevelWithParamsSelector
  , readAttributeBatReplacementNeededWithParamsSelector
  , readAttributeBatReplaceabilityWithParamsSelector
  , readAttributeBatPresentWithParamsSelector
  , readAttributeActiveBatFaultsWithParamsSelector
  , readAttributeBatReplacementDescriptionWithParamsSelector
  , readAttributeBatCommonDesignationWithParamsSelector
  , readAttributeBatANSIDesignationWithParamsSelector
  , readAttributeBatIECDesignationWithParamsSelector
  , readAttributeBatApprovedChemistryWithParamsSelector
  , readAttributeBatCapacityWithParamsSelector
  , readAttributeBatQuantityWithParamsSelector
  , readAttributeBatChargeStateWithParamsSelector
  , readAttributeBatTimeToFullChargeWithParamsSelector
  , readAttributeBatFunctionalWhileChargingWithParamsSelector
  , readAttributeBatChargingCurrentWithParamsSelector
  , readAttributeActiveBatChargeFaultsWithParamsSelector
  , readAttributeEndpointListWithParamsSelector
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

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOrderWithParams:@
readAttributeOrderWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeOrderWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeOrderWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeDescriptionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredAssessedInputVoltageWithParams:@
readAttributeWiredAssessedInputVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedInputVoltageWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredAssessedInputVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredAssessedInputFrequencyWithParams:@
readAttributeWiredAssessedInputFrequencyWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedInputFrequencyWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredAssessedInputFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredCurrentTypeWithParams:@
readAttributeWiredCurrentTypeWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredCurrentTypeWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredCurrentTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredAssessedCurrentWithParams:@
readAttributeWiredAssessedCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedCurrentWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredAssessedCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredNominalVoltageWithParams:@
readAttributeWiredNominalVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredNominalVoltageWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredNominalVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredMaximumCurrentWithParams:@
readAttributeWiredMaximumCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredMaximumCurrentWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredMaximumCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWiredPresentWithParams:@
readAttributeWiredPresentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredPresentWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeWiredPresentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveWiredFaultsWithParams:@
readAttributeActiveWiredFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveWiredFaultsWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeActiveWiredFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatVoltageWithParams:@
readAttributeBatVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatVoltageWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatPercentRemainingWithParams:@
readAttributeBatPercentRemainingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatPercentRemainingWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatPercentRemainingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatTimeRemainingWithParams:@
readAttributeBatTimeRemainingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatTimeRemainingWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatTimeRemainingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatChargeLevelWithParams:@
readAttributeBatChargeLevelWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargeLevelWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatChargeLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatReplacementNeededWithParams:@
readAttributeBatReplacementNeededWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplacementNeededWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatReplacementNeededWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatReplaceabilityWithParams:@
readAttributeBatReplaceabilityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplaceabilityWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatReplaceabilityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatPresentWithParams:@
readAttributeBatPresentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatPresentWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatPresentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveBatFaultsWithParams:@
readAttributeActiveBatFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveBatFaultsWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeActiveBatFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatReplacementDescriptionWithParams:@
readAttributeBatReplacementDescriptionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplacementDescriptionWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatReplacementDescriptionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatCommonDesignationWithParams:@
readAttributeBatCommonDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatCommonDesignationWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatCommonDesignationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatANSIDesignationWithParams:@
readAttributeBatANSIDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatANSIDesignationWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatANSIDesignationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatIECDesignationWithParams:@
readAttributeBatIECDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatIECDesignationWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatIECDesignationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatApprovedChemistryWithParams:@
readAttributeBatApprovedChemistryWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatApprovedChemistryWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatApprovedChemistryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatCapacityWithParams:@
readAttributeBatCapacityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatCapacityWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatCapacityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatQuantityWithParams:@
readAttributeBatQuantityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatQuantityWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatQuantityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatChargeStateWithParams:@
readAttributeBatChargeStateWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargeStateWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatChargeStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatTimeToFullChargeWithParams:@
readAttributeBatTimeToFullChargeWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatTimeToFullChargeWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatTimeToFullChargeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatFunctionalWhileChargingWithParams:@
readAttributeBatFunctionalWhileChargingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatFunctionalWhileChargingWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatFunctionalWhileChargingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatChargingCurrentWithParams:@
readAttributeBatChargingCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargingCurrentWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeBatChargingCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveBatChargeFaultsWithParams:@
readAttributeActiveBatChargeFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveBatChargeFaultsWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeActiveBatChargeFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndpointListWithParams:@
readAttributeEndpointListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeEndpointListWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeEndpointListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPowerSource  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSource (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPowerSource mtrClusterPowerSource => mtrClusterPowerSource -> IO (Id MTRClusterPowerSource)
init_ mtrClusterPowerSource  =
    sendMsg mtrClusterPowerSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPowerSource)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPowerSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRDevice device, IsNSObject queue) => mtrClusterPowerSource -> device -> CUShort -> queue -> IO (Id MTRClusterPowerSource)
initWithDevice_endpoint_queue mtrClusterPowerSource  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterPowerSource (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPowerSource -> device -> endpointID -> queue -> IO (Id MTRClusterPowerSource)
initWithDevice_endpointID_queue mtrClusterPowerSource  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPowerSource (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeOrderWithParams:@
readAttributeOrderWithParamsSelector :: Selector
readAttributeOrderWithParamsSelector = mkSelector "readAttributeOrderWithParams:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedInputVoltageWithParams:@
readAttributeWiredAssessedInputVoltageWithParamsSelector :: Selector
readAttributeWiredAssessedInputVoltageWithParamsSelector = mkSelector "readAttributeWiredAssessedInputVoltageWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedInputFrequencyWithParams:@
readAttributeWiredAssessedInputFrequencyWithParamsSelector :: Selector
readAttributeWiredAssessedInputFrequencyWithParamsSelector = mkSelector "readAttributeWiredAssessedInputFrequencyWithParams:"

-- | @Selector@ for @readAttributeWiredCurrentTypeWithParams:@
readAttributeWiredCurrentTypeWithParamsSelector :: Selector
readAttributeWiredCurrentTypeWithParamsSelector = mkSelector "readAttributeWiredCurrentTypeWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedCurrentWithParams:@
readAttributeWiredAssessedCurrentWithParamsSelector :: Selector
readAttributeWiredAssessedCurrentWithParamsSelector = mkSelector "readAttributeWiredAssessedCurrentWithParams:"

-- | @Selector@ for @readAttributeWiredNominalVoltageWithParams:@
readAttributeWiredNominalVoltageWithParamsSelector :: Selector
readAttributeWiredNominalVoltageWithParamsSelector = mkSelector "readAttributeWiredNominalVoltageWithParams:"

-- | @Selector@ for @readAttributeWiredMaximumCurrentWithParams:@
readAttributeWiredMaximumCurrentWithParamsSelector :: Selector
readAttributeWiredMaximumCurrentWithParamsSelector = mkSelector "readAttributeWiredMaximumCurrentWithParams:"

-- | @Selector@ for @readAttributeWiredPresentWithParams:@
readAttributeWiredPresentWithParamsSelector :: Selector
readAttributeWiredPresentWithParamsSelector = mkSelector "readAttributeWiredPresentWithParams:"

-- | @Selector@ for @readAttributeActiveWiredFaultsWithParams:@
readAttributeActiveWiredFaultsWithParamsSelector :: Selector
readAttributeActiveWiredFaultsWithParamsSelector = mkSelector "readAttributeActiveWiredFaultsWithParams:"

-- | @Selector@ for @readAttributeBatVoltageWithParams:@
readAttributeBatVoltageWithParamsSelector :: Selector
readAttributeBatVoltageWithParamsSelector = mkSelector "readAttributeBatVoltageWithParams:"

-- | @Selector@ for @readAttributeBatPercentRemainingWithParams:@
readAttributeBatPercentRemainingWithParamsSelector :: Selector
readAttributeBatPercentRemainingWithParamsSelector = mkSelector "readAttributeBatPercentRemainingWithParams:"

-- | @Selector@ for @readAttributeBatTimeRemainingWithParams:@
readAttributeBatTimeRemainingWithParamsSelector :: Selector
readAttributeBatTimeRemainingWithParamsSelector = mkSelector "readAttributeBatTimeRemainingWithParams:"

-- | @Selector@ for @readAttributeBatChargeLevelWithParams:@
readAttributeBatChargeLevelWithParamsSelector :: Selector
readAttributeBatChargeLevelWithParamsSelector = mkSelector "readAttributeBatChargeLevelWithParams:"

-- | @Selector@ for @readAttributeBatReplacementNeededWithParams:@
readAttributeBatReplacementNeededWithParamsSelector :: Selector
readAttributeBatReplacementNeededWithParamsSelector = mkSelector "readAttributeBatReplacementNeededWithParams:"

-- | @Selector@ for @readAttributeBatReplaceabilityWithParams:@
readAttributeBatReplaceabilityWithParamsSelector :: Selector
readAttributeBatReplaceabilityWithParamsSelector = mkSelector "readAttributeBatReplaceabilityWithParams:"

-- | @Selector@ for @readAttributeBatPresentWithParams:@
readAttributeBatPresentWithParamsSelector :: Selector
readAttributeBatPresentWithParamsSelector = mkSelector "readAttributeBatPresentWithParams:"

-- | @Selector@ for @readAttributeActiveBatFaultsWithParams:@
readAttributeActiveBatFaultsWithParamsSelector :: Selector
readAttributeActiveBatFaultsWithParamsSelector = mkSelector "readAttributeActiveBatFaultsWithParams:"

-- | @Selector@ for @readAttributeBatReplacementDescriptionWithParams:@
readAttributeBatReplacementDescriptionWithParamsSelector :: Selector
readAttributeBatReplacementDescriptionWithParamsSelector = mkSelector "readAttributeBatReplacementDescriptionWithParams:"

-- | @Selector@ for @readAttributeBatCommonDesignationWithParams:@
readAttributeBatCommonDesignationWithParamsSelector :: Selector
readAttributeBatCommonDesignationWithParamsSelector = mkSelector "readAttributeBatCommonDesignationWithParams:"

-- | @Selector@ for @readAttributeBatANSIDesignationWithParams:@
readAttributeBatANSIDesignationWithParamsSelector :: Selector
readAttributeBatANSIDesignationWithParamsSelector = mkSelector "readAttributeBatANSIDesignationWithParams:"

-- | @Selector@ for @readAttributeBatIECDesignationWithParams:@
readAttributeBatIECDesignationWithParamsSelector :: Selector
readAttributeBatIECDesignationWithParamsSelector = mkSelector "readAttributeBatIECDesignationWithParams:"

-- | @Selector@ for @readAttributeBatApprovedChemistryWithParams:@
readAttributeBatApprovedChemistryWithParamsSelector :: Selector
readAttributeBatApprovedChemistryWithParamsSelector = mkSelector "readAttributeBatApprovedChemistryWithParams:"

-- | @Selector@ for @readAttributeBatCapacityWithParams:@
readAttributeBatCapacityWithParamsSelector :: Selector
readAttributeBatCapacityWithParamsSelector = mkSelector "readAttributeBatCapacityWithParams:"

-- | @Selector@ for @readAttributeBatQuantityWithParams:@
readAttributeBatQuantityWithParamsSelector :: Selector
readAttributeBatQuantityWithParamsSelector = mkSelector "readAttributeBatQuantityWithParams:"

-- | @Selector@ for @readAttributeBatChargeStateWithParams:@
readAttributeBatChargeStateWithParamsSelector :: Selector
readAttributeBatChargeStateWithParamsSelector = mkSelector "readAttributeBatChargeStateWithParams:"

-- | @Selector@ for @readAttributeBatTimeToFullChargeWithParams:@
readAttributeBatTimeToFullChargeWithParamsSelector :: Selector
readAttributeBatTimeToFullChargeWithParamsSelector = mkSelector "readAttributeBatTimeToFullChargeWithParams:"

-- | @Selector@ for @readAttributeBatFunctionalWhileChargingWithParams:@
readAttributeBatFunctionalWhileChargingWithParamsSelector :: Selector
readAttributeBatFunctionalWhileChargingWithParamsSelector = mkSelector "readAttributeBatFunctionalWhileChargingWithParams:"

-- | @Selector@ for @readAttributeBatChargingCurrentWithParams:@
readAttributeBatChargingCurrentWithParamsSelector :: Selector
readAttributeBatChargingCurrentWithParamsSelector = mkSelector "readAttributeBatChargingCurrentWithParams:"

-- | @Selector@ for @readAttributeActiveBatChargeFaultsWithParams:@
readAttributeActiveBatChargeFaultsWithParamsSelector :: Selector
readAttributeActiveBatChargeFaultsWithParamsSelector = mkSelector "readAttributeActiveBatChargeFaultsWithParams:"

-- | @Selector@ for @readAttributeEndpointListWithParams:@
readAttributeEndpointListWithParamsSelector :: Selector
readAttributeEndpointListWithParamsSelector = mkSelector "readAttributeEndpointListWithParams:"

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

