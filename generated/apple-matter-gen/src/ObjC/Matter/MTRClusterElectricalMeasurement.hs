{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Measurement    Attributes related to the electrical properties of a device. This cluster is used by power outlets and other devices that need to provide instantaneous data as opposed to metrology data which should be retrieved from the metering cluster..
--
-- Generated bindings for @MTRClusterElectricalMeasurement@.
module ObjC.Matter.MTRClusterElectricalMeasurement
  ( MTRClusterElectricalMeasurement
  , IsMTRClusterElectricalMeasurement(..)
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMeasurementTypeWithParams
  , readAttributeDcVoltageWithParams
  , readAttributeDcVoltageMinWithParams
  , readAttributeDcVoltageMaxWithParams
  , readAttributeDcCurrentWithParams
  , readAttributeDcCurrentMinWithParams
  , readAttributeDcCurrentMaxWithParams
  , readAttributeDcPowerWithParams
  , readAttributeDcPowerMinWithParams
  , readAttributeDcPowerMaxWithParams
  , readAttributeDcVoltageMultiplierWithParams
  , readAttributeDcVoltageDivisorWithParams
  , readAttributeDcCurrentMultiplierWithParams
  , readAttributeDcCurrentDivisorWithParams
  , readAttributeDcPowerMultiplierWithParams
  , readAttributeDcPowerDivisorWithParams
  , readAttributeAcFrequencyWithParams
  , readAttributeAcFrequencyMinWithParams
  , readAttributeAcFrequencyMaxWithParams
  , readAttributeNeutralCurrentWithParams
  , readAttributeTotalActivePowerWithParams
  , readAttributeTotalReactivePowerWithParams
  , readAttributeTotalApparentPowerWithParams
  , readAttributeMeasured1stHarmonicCurrentWithParams
  , readAttributeMeasured3rdHarmonicCurrentWithParams
  , readAttributeMeasured5thHarmonicCurrentWithParams
  , readAttributeMeasured7thHarmonicCurrentWithParams
  , readAttributeMeasured9thHarmonicCurrentWithParams
  , readAttributeMeasured11thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase1stHarmonicCurrentWithParams
  , readAttributeMeasuredPhase3rdHarmonicCurrentWithParams
  , readAttributeMeasuredPhase5thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase7thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase9thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase11thHarmonicCurrentWithParams
  , readAttributeAcFrequencyMultiplierWithParams
  , readAttributeAcFrequencyDivisorWithParams
  , readAttributePowerMultiplierWithParams
  , readAttributePowerDivisorWithParams
  , readAttributeHarmonicCurrentMultiplierWithParams
  , readAttributePhaseHarmonicCurrentMultiplierWithParams
  , readAttributeInstantaneousVoltageWithParams
  , readAttributeInstantaneousLineCurrentWithParams
  , readAttributeInstantaneousActiveCurrentWithParams
  , readAttributeInstantaneousReactiveCurrentWithParams
  , readAttributeInstantaneousPowerWithParams
  , readAttributeRmsVoltageWithParams
  , readAttributeRmsVoltageMinWithParams
  , readAttributeRmsVoltageMaxWithParams
  , readAttributeRmsCurrentWithParams
  , readAttributeRmsCurrentMinWithParams
  , readAttributeRmsCurrentMaxWithParams
  , readAttributeActivePowerWithParams
  , readAttributeActivePowerMinWithParams
  , readAttributeActivePowerMaxWithParams
  , readAttributeReactivePowerWithParams
  , readAttributeApparentPowerWithParams
  , readAttributePowerFactorWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodWithParams
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params
  , readAttributeAverageRmsUnderVoltageCounterWithParams
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params
  , readAttributeRmsExtremeOverVoltagePeriodWithParams
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params
  , readAttributeRmsExtremeUnderVoltagePeriodWithParams
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params
  , readAttributeRmsVoltageSagPeriodWithParams
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params
  , readAttributeRmsVoltageSwellPeriodWithParams
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params
  , readAttributeAcVoltageMultiplierWithParams
  , readAttributeAcVoltageDivisorWithParams
  , readAttributeAcCurrentMultiplierWithParams
  , readAttributeAcCurrentDivisorWithParams
  , readAttributeAcPowerMultiplierWithParams
  , readAttributeAcPowerDivisorWithParams
  , readAttributeOverloadAlarmsMaskWithParams
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params
  , readAttributeVoltageOverloadWithParams
  , readAttributeCurrentOverloadWithParams
  , readAttributeAcOverloadAlarmsMaskWithParams
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params
  , readAttributeAcVoltageOverloadWithParams
  , readAttributeAcCurrentOverloadWithParams
  , readAttributeAcActivePowerOverloadWithParams
  , readAttributeAcReactivePowerOverloadWithParams
  , readAttributeAverageRmsOverVoltageWithParams
  , readAttributeAverageRmsUnderVoltageWithParams
  , readAttributeRmsExtremeOverVoltageWithParams
  , readAttributeRmsExtremeUnderVoltageWithParams
  , readAttributeRmsVoltageSagWithParams
  , readAttributeRmsVoltageSwellWithParams
  , readAttributeLineCurrentPhaseBWithParams
  , readAttributeActiveCurrentPhaseBWithParams
  , readAttributeReactiveCurrentPhaseBWithParams
  , readAttributeRmsVoltagePhaseBWithParams
  , readAttributeRmsVoltageMinPhaseBWithParams
  , readAttributeRmsVoltageMaxPhaseBWithParams
  , readAttributeRmsCurrentPhaseBWithParams
  , readAttributeRmsCurrentMinPhaseBWithParams
  , readAttributeRmsCurrentMaxPhaseBWithParams
  , readAttributeActivePowerPhaseBWithParams
  , readAttributeActivePowerMinPhaseBWithParams
  , readAttributeActivePowerMaxPhaseBWithParams
  , readAttributeReactivePowerPhaseBWithParams
  , readAttributeApparentPowerPhaseBWithParams
  , readAttributePowerFactorPhaseBWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams
  , readAttributeAverageRmsOverVoltageCounterPhaseBWithParams
  , readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams
  , readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams
  , readAttributeRmsVoltageSagPeriodPhaseBWithParams
  , readAttributeRmsVoltageSwellPeriodPhaseBWithParams
  , readAttributeLineCurrentPhaseCWithParams
  , readAttributeActiveCurrentPhaseCWithParams
  , readAttributeReactiveCurrentPhaseCWithParams
  , readAttributeRmsVoltagePhaseCWithParams
  , readAttributeRmsVoltageMinPhaseCWithParams
  , readAttributeRmsVoltageMaxPhaseCWithParams
  , readAttributeRmsCurrentPhaseCWithParams
  , readAttributeRmsCurrentMinPhaseCWithParams
  , readAttributeRmsCurrentMaxPhaseCWithParams
  , readAttributeActivePowerPhaseCWithParams
  , readAttributeActivePowerMinPhaseCWithParams
  , readAttributeActivePowerMaxPhaseCWithParams
  , readAttributeReactivePowerPhaseCWithParams
  , readAttributeApparentPowerPhaseCWithParams
  , readAttributePowerFactorPhaseCWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams
  , readAttributeAverageRmsOverVoltageCounterPhaseCWithParams
  , readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams
  , readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams
  , readAttributeRmsVoltageSagPeriodPhaseCWithParams
  , readAttributeRmsVoltageSwellPeriodPhaseCWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMeasurementTypeWithParamsSelector
  , readAttributeDcVoltageWithParamsSelector
  , readAttributeDcVoltageMinWithParamsSelector
  , readAttributeDcVoltageMaxWithParamsSelector
  , readAttributeDcCurrentWithParamsSelector
  , readAttributeDcCurrentMinWithParamsSelector
  , readAttributeDcCurrentMaxWithParamsSelector
  , readAttributeDcPowerWithParamsSelector
  , readAttributeDcPowerMinWithParamsSelector
  , readAttributeDcPowerMaxWithParamsSelector
  , readAttributeDcVoltageMultiplierWithParamsSelector
  , readAttributeDcVoltageDivisorWithParamsSelector
  , readAttributeDcCurrentMultiplierWithParamsSelector
  , readAttributeDcCurrentDivisorWithParamsSelector
  , readAttributeDcPowerMultiplierWithParamsSelector
  , readAttributeDcPowerDivisorWithParamsSelector
  , readAttributeAcFrequencyWithParamsSelector
  , readAttributeAcFrequencyMinWithParamsSelector
  , readAttributeAcFrequencyMaxWithParamsSelector
  , readAttributeNeutralCurrentWithParamsSelector
  , readAttributeTotalActivePowerWithParamsSelector
  , readAttributeTotalReactivePowerWithParamsSelector
  , readAttributeTotalApparentPowerWithParamsSelector
  , readAttributeMeasured1stHarmonicCurrentWithParamsSelector
  , readAttributeMeasured3rdHarmonicCurrentWithParamsSelector
  , readAttributeMeasured5thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured7thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured9thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured11thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector
  , readAttributeAcFrequencyMultiplierWithParamsSelector
  , readAttributeAcFrequencyDivisorWithParamsSelector
  , readAttributePowerMultiplierWithParamsSelector
  , readAttributePowerDivisorWithParamsSelector
  , readAttributeHarmonicCurrentMultiplierWithParamsSelector
  , readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector
  , readAttributeInstantaneousVoltageWithParamsSelector
  , readAttributeInstantaneousLineCurrentWithParamsSelector
  , readAttributeInstantaneousActiveCurrentWithParamsSelector
  , readAttributeInstantaneousReactiveCurrentWithParamsSelector
  , readAttributeInstantaneousPowerWithParamsSelector
  , readAttributeRmsVoltageWithParamsSelector
  , readAttributeRmsVoltageMinWithParamsSelector
  , readAttributeRmsVoltageMaxWithParamsSelector
  , readAttributeRmsCurrentWithParamsSelector
  , readAttributeRmsCurrentMinWithParamsSelector
  , readAttributeRmsCurrentMaxWithParamsSelector
  , readAttributeActivePowerWithParamsSelector
  , readAttributeActivePowerMinWithParamsSelector
  , readAttributeActivePowerMaxWithParamsSelector
  , readAttributeReactivePowerWithParamsSelector
  , readAttributeApparentPowerWithParamsSelector
  , readAttributePowerFactorWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeAverageRmsUnderVoltageCounterWithParamsSelector
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector
  , readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeRmsVoltageSagPeriodWithParamsSelector
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeRmsVoltageSwellPeriodWithParamsSelector
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeAcVoltageMultiplierWithParamsSelector
  , readAttributeAcVoltageDivisorWithParamsSelector
  , readAttributeAcCurrentMultiplierWithParamsSelector
  , readAttributeAcCurrentDivisorWithParamsSelector
  , readAttributeAcPowerMultiplierWithParamsSelector
  , readAttributeAcPowerDivisorWithParamsSelector
  , readAttributeOverloadAlarmsMaskWithParamsSelector
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector
  , readAttributeVoltageOverloadWithParamsSelector
  , readAttributeCurrentOverloadWithParamsSelector
  , readAttributeAcOverloadAlarmsMaskWithParamsSelector
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector
  , readAttributeAcVoltageOverloadWithParamsSelector
  , readAttributeAcCurrentOverloadWithParamsSelector
  , readAttributeAcActivePowerOverloadWithParamsSelector
  , readAttributeAcReactivePowerOverloadWithParamsSelector
  , readAttributeAverageRmsOverVoltageWithParamsSelector
  , readAttributeAverageRmsUnderVoltageWithParamsSelector
  , readAttributeRmsExtremeOverVoltageWithParamsSelector
  , readAttributeRmsExtremeUnderVoltageWithParamsSelector
  , readAttributeRmsVoltageSagWithParamsSelector
  , readAttributeRmsVoltageSwellWithParamsSelector
  , readAttributeLineCurrentPhaseBWithParamsSelector
  , readAttributeActiveCurrentPhaseBWithParamsSelector
  , readAttributeReactiveCurrentPhaseBWithParamsSelector
  , readAttributeRmsVoltagePhaseBWithParamsSelector
  , readAttributeRmsVoltageMinPhaseBWithParamsSelector
  , readAttributeRmsVoltageMaxPhaseBWithParamsSelector
  , readAttributeRmsCurrentPhaseBWithParamsSelector
  , readAttributeRmsCurrentMinPhaseBWithParamsSelector
  , readAttributeRmsCurrentMaxPhaseBWithParamsSelector
  , readAttributeActivePowerPhaseBWithParamsSelector
  , readAttributeActivePowerMinPhaseBWithParamsSelector
  , readAttributeActivePowerMaxPhaseBWithParamsSelector
  , readAttributeReactivePowerPhaseBWithParamsSelector
  , readAttributeApparentPowerPhaseBWithParamsSelector
  , readAttributePowerFactorPhaseBWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector
  , readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector
  , readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector
  , readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector
  , readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector
  , readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector
  , readAttributeLineCurrentPhaseCWithParamsSelector
  , readAttributeActiveCurrentPhaseCWithParamsSelector
  , readAttributeReactiveCurrentPhaseCWithParamsSelector
  , readAttributeRmsVoltagePhaseCWithParamsSelector
  , readAttributeRmsVoltageMinPhaseCWithParamsSelector
  , readAttributeRmsVoltageMaxPhaseCWithParamsSelector
  , readAttributeRmsCurrentPhaseCWithParamsSelector
  , readAttributeRmsCurrentMinPhaseCWithParamsSelector
  , readAttributeRmsCurrentMaxPhaseCWithParamsSelector
  , readAttributeActivePowerPhaseCWithParamsSelector
  , readAttributeActivePowerMinPhaseCWithParamsSelector
  , readAttributeActivePowerMaxPhaseCWithParamsSelector
  , readAttributeReactivePowerPhaseCWithParamsSelector
  , readAttributeApparentPowerPhaseCWithParamsSelector
  , readAttributePowerFactorPhaseCWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector
  , readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector
  , readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector
  , readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector
  , readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector
  , readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetProfileInfoCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMeasurementTypeWithParams:@
readAttributeMeasurementTypeWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementTypeWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasurementTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcVoltageWithParams:@
readAttributeDcVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcVoltageMinWithParams:@
readAttributeDcVoltageMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcVoltageMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcVoltageMaxWithParams:@
readAttributeDcVoltageMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcVoltageMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcCurrentWithParams:@
readAttributeDcCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcCurrentMinWithParams:@
readAttributeDcCurrentMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcCurrentMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcCurrentMaxWithParams:@
readAttributeDcCurrentMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcCurrentMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcPowerWithParams:@
readAttributeDcPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcPowerMinWithParams:@
readAttributeDcPowerMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcPowerMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcPowerMaxWithParams:@
readAttributeDcPowerMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcPowerMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcVoltageMultiplierWithParams:@
readAttributeDcVoltageMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcVoltageMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcVoltageDivisorWithParams:@
readAttributeDcVoltageDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcVoltageDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcCurrentMultiplierWithParams:@
readAttributeDcCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcCurrentMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcCurrentDivisorWithParams:@
readAttributeDcCurrentDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcCurrentDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcPowerMultiplierWithParams:@
readAttributeDcPowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcPowerMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDcPowerDivisorWithParams:@
readAttributeDcPowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeDcPowerDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcFrequencyWithParams:@
readAttributeAcFrequencyWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcFrequencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcFrequencyMinWithParams:@
readAttributeAcFrequencyMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcFrequencyMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcFrequencyMaxWithParams:@
readAttributeAcFrequencyMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcFrequencyMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeNeutralCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeNeutralCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTotalActivePowerWithParams:@
readAttributeTotalActivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalActivePowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeTotalActivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTotalReactivePowerWithParams:@
readAttributeTotalReactivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalReactivePowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeTotalReactivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTotalApparentPowerWithParams:@
readAttributeTotalApparentPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalApparentPowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeTotalApparentPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured1stHarmonicCurrentWithParams:@
readAttributeMeasured1stHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured1stHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured1stHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured3rdHarmonicCurrentWithParams:@
readAttributeMeasured3rdHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured3rdHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured3rdHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured5thHarmonicCurrentWithParams:@
readAttributeMeasured5thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured5thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured5thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured7thHarmonicCurrentWithParams:@
readAttributeMeasured7thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured7thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured7thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured9thHarmonicCurrentWithParams:@
readAttributeMeasured9thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured9thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured9thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasured11thHarmonicCurrentWithParams:@
readAttributeMeasured11thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured11thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasured11thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase1stHarmonicCurrentWithParams:@
readAttributeMeasuredPhase1stHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase1stHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase1stHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:@
readAttributeMeasuredPhase3rdHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase3rdHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase5thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase5thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase5thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase5thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase7thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase7thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase7thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase7thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase9thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase9thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase9thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase9thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeasuredPhase11thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase11thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase11thHarmonicCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeMeasuredPhase11thHarmonicCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcFrequencyMultiplierWithParams:@
readAttributeAcFrequencyMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcFrequencyMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcFrequencyDivisorWithParams:@
readAttributeAcFrequencyDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcFrequencyDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerMultiplierWithParams:@
readAttributePowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePowerMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerDivisorWithParams:@
readAttributePowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePowerDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHarmonicCurrentMultiplierWithParams:@
readAttributeHarmonicCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicCurrentMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeHarmonicCurrentMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePhaseHarmonicCurrentMultiplierWithParams:@
readAttributePhaseHarmonicCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePhaseHarmonicCurrentMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePhaseHarmonicCurrentMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstantaneousVoltageWithParams:@
readAttributeInstantaneousVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeInstantaneousVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstantaneousLineCurrentWithParams:@
readAttributeInstantaneousLineCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousLineCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeInstantaneousLineCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstantaneousActiveCurrentWithParams:@
readAttributeInstantaneousActiveCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousActiveCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeInstantaneousActiveCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstantaneousReactiveCurrentWithParams:@
readAttributeInstantaneousReactiveCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousReactiveCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeInstantaneousReactiveCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInstantaneousPowerWithParams:@
readAttributeInstantaneousPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousPowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeInstantaneousPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageWithParams:@
readAttributeRmsVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMinWithParams:@
readAttributeRmsVoltageMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMaxWithParams:@
readAttributeRmsVoltageMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentWithParams:@
readAttributeRmsCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMinWithParams:@
readAttributeRmsCurrentMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMaxWithParams:@
readAttributeRmsCurrentMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMinWithParams:@
readAttributeActivePowerMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMaxWithParams:@
readAttributeActivePowerMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeReactivePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeApparentPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePowerFactorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAverageRmsUnderVoltageCounterWithParams:@
readAttributeAverageRmsUnderVoltageCounterWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsUnderVoltageCounterWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRmsExtremeOverVoltagePeriodWithParams:@
readAttributeRmsExtremeOverVoltagePeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeOverVoltagePeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRmsExtremeUnderVoltagePeriodWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeUnderVoltagePeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRmsVoltageSagPeriodWithParams:@
readAttributeRmsVoltageSagPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSagPeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRmsVoltageSwellPeriodWithParams:@
readAttributeRmsVoltageSwellPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSwellPeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAcVoltageMultiplierWithParams:@
readAttributeAcVoltageMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcVoltageMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcVoltageDivisorWithParams:@
readAttributeAcVoltageDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcVoltageDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcCurrentMultiplierWithParams:@
readAttributeAcCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcCurrentMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcCurrentDivisorWithParams:@
readAttributeAcCurrentDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcCurrentDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcPowerMultiplierWithParams:@
readAttributeAcPowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcPowerMultiplierWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcPowerMultiplierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcPowerDivisorWithParams:@
readAttributeAcPowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcPowerDivisorWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcPowerDivisorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverloadAlarmsMaskWithParams:@
readAttributeOverloadAlarmsMaskWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeOverloadAlarmsMaskWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeOverloadAlarmsMaskWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeVoltageOverloadWithParams:@
readAttributeVoltageOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeVoltageOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeVoltageOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentOverloadWithParams:@
readAttributeCurrentOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeCurrentOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeCurrentOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcOverloadAlarmsMaskWithParams:@
readAttributeAcOverloadAlarmsMaskWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcOverloadAlarmsMaskWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcOverloadAlarmsMaskWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAcVoltageOverloadWithParams:@
readAttributeAcVoltageOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcVoltageOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcCurrentOverloadWithParams:@
readAttributeAcCurrentOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcCurrentOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcActivePowerOverloadWithParams:@
readAttributeAcActivePowerOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcActivePowerOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcActivePowerOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcReactivePowerOverloadWithParams:@
readAttributeAcReactivePowerOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcReactivePowerOverloadWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcReactivePowerOverloadWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsOverVoltageWithParams:@
readAttributeAverageRmsOverVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsOverVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsUnderVoltageWithParams:@
readAttributeAverageRmsUnderVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsUnderVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeOverVoltageWithParams:@
readAttributeRmsExtremeOverVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeOverVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeUnderVoltageWithParams:@
readAttributeRmsExtremeUnderVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltageWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeUnderVoltageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSagWithParams:@
readAttributeRmsVoltageSagWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSagWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSwellWithParams:@
readAttributeRmsVoltageSwellWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSwellWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLineCurrentPhaseBWithParams:@
readAttributeLineCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeLineCurrentPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeLineCurrentPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveCurrentPhaseBWithParams:@
readAttributeActiveCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActiveCurrentPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactiveCurrentPhaseBWithParams:@
readAttributeReactiveCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeReactiveCurrentPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltagePhaseBWithParams:@
readAttributeRmsVoltagePhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltagePhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltagePhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMinPhaseBWithParams:@
readAttributeRmsVoltageMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMinPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMaxPhaseBWithParams:@
readAttributeRmsVoltageMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMaxPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentPhaseBWithParams:@
readAttributeRmsCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMinPhaseBWithParams:@
readAttributeRmsCurrentMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMinPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMaxPhaseBWithParams:@
readAttributeRmsCurrentMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMaxPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerPhaseBWithParams:@
readAttributeActivePowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMinPhaseBWithParams:@
readAttributeActivePowerMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMinPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMaxPhaseBWithParams:@
readAttributeActivePowerMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMaxPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactivePowerPhaseBWithParams:@
readAttributeReactivePowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeReactivePowerPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApparentPowerPhaseBWithParams:@
readAttributeApparentPowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeApparentPowerPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerFactorPhaseBWithParams:@
readAttributePowerFactorPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePowerFactorPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSagPeriodPhaseBWithParams:@
readAttributeRmsVoltageSagPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSagPeriodPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSwellPeriodPhaseBWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseBWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSwellPeriodPhaseBWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLineCurrentPhaseCWithParams:@
readAttributeLineCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeLineCurrentPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeLineCurrentPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveCurrentPhaseCWithParams:@
readAttributeActiveCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActiveCurrentPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactiveCurrentPhaseCWithParams:@
readAttributeReactiveCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeReactiveCurrentPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltagePhaseCWithParams:@
readAttributeRmsVoltagePhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltagePhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltagePhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMinPhaseCWithParams:@
readAttributeRmsVoltageMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMinPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageMaxPhaseCWithParams:@
readAttributeRmsVoltageMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageMaxPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentPhaseCWithParams:@
readAttributeRmsCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMinPhaseCWithParams:@
readAttributeRmsCurrentMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMinPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsCurrentMaxPhaseCWithParams:@
readAttributeRmsCurrentMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsCurrentMaxPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerPhaseCWithParams:@
readAttributeActivePowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMinPhaseCWithParams:@
readAttributeActivePowerMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMinPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePowerMaxPhaseCWithParams:@
readAttributeActivePowerMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeActivePowerMaxPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeReactivePowerPhaseCWithParams:@
readAttributeReactivePowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeReactivePowerPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApparentPowerPhaseCWithParams:@
readAttributeApparentPowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeApparentPowerPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerFactorPhaseCWithParams:@
readAttributePowerFactorPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributePowerFactorPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSagPeriodPhaseCWithParams:@
readAttributeRmsVoltageSagPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSagPeriodPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRmsVoltageSwellPeriodPhaseCWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseCWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeRmsVoltageSwellPeriodPhaseCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterElectricalMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement => mtrClusterElectricalMeasurement -> IO (Id MTRClusterElectricalMeasurement)
init_ mtrClusterElectricalMeasurement  =
    sendMsg mtrClusterElectricalMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterElectricalMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterElectricalMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterElectricalMeasurement)
initWithDevice_endpoint_queue mtrClusterElectricalMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetProfileInfoCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterElectricalMeasurement (mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterElectricalMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector :: Selector
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMeasurementTypeWithParams:@
readAttributeMeasurementTypeWithParamsSelector :: Selector
readAttributeMeasurementTypeWithParamsSelector = mkSelector "readAttributeMeasurementTypeWithParams:"

-- | @Selector@ for @readAttributeDcVoltageWithParams:@
readAttributeDcVoltageWithParamsSelector :: Selector
readAttributeDcVoltageWithParamsSelector = mkSelector "readAttributeDcVoltageWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMinWithParams:@
readAttributeDcVoltageMinWithParamsSelector :: Selector
readAttributeDcVoltageMinWithParamsSelector = mkSelector "readAttributeDcVoltageMinWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMaxWithParams:@
readAttributeDcVoltageMaxWithParamsSelector :: Selector
readAttributeDcVoltageMaxWithParamsSelector = mkSelector "readAttributeDcVoltageMaxWithParams:"

-- | @Selector@ for @readAttributeDcCurrentWithParams:@
readAttributeDcCurrentWithParamsSelector :: Selector
readAttributeDcCurrentWithParamsSelector = mkSelector "readAttributeDcCurrentWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMinWithParams:@
readAttributeDcCurrentMinWithParamsSelector :: Selector
readAttributeDcCurrentMinWithParamsSelector = mkSelector "readAttributeDcCurrentMinWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMaxWithParams:@
readAttributeDcCurrentMaxWithParamsSelector :: Selector
readAttributeDcCurrentMaxWithParamsSelector = mkSelector "readAttributeDcCurrentMaxWithParams:"

-- | @Selector@ for @readAttributeDcPowerWithParams:@
readAttributeDcPowerWithParamsSelector :: Selector
readAttributeDcPowerWithParamsSelector = mkSelector "readAttributeDcPowerWithParams:"

-- | @Selector@ for @readAttributeDcPowerMinWithParams:@
readAttributeDcPowerMinWithParamsSelector :: Selector
readAttributeDcPowerMinWithParamsSelector = mkSelector "readAttributeDcPowerMinWithParams:"

-- | @Selector@ for @readAttributeDcPowerMaxWithParams:@
readAttributeDcPowerMaxWithParamsSelector :: Selector
readAttributeDcPowerMaxWithParamsSelector = mkSelector "readAttributeDcPowerMaxWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMultiplierWithParams:@
readAttributeDcVoltageMultiplierWithParamsSelector :: Selector
readAttributeDcVoltageMultiplierWithParamsSelector = mkSelector "readAttributeDcVoltageMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcVoltageDivisorWithParams:@
readAttributeDcVoltageDivisorWithParamsSelector :: Selector
readAttributeDcVoltageDivisorWithParamsSelector = mkSelector "readAttributeDcVoltageDivisorWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMultiplierWithParams:@
readAttributeDcCurrentMultiplierWithParamsSelector :: Selector
readAttributeDcCurrentMultiplierWithParamsSelector = mkSelector "readAttributeDcCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcCurrentDivisorWithParams:@
readAttributeDcCurrentDivisorWithParamsSelector :: Selector
readAttributeDcCurrentDivisorWithParamsSelector = mkSelector "readAttributeDcCurrentDivisorWithParams:"

-- | @Selector@ for @readAttributeDcPowerMultiplierWithParams:@
readAttributeDcPowerMultiplierWithParamsSelector :: Selector
readAttributeDcPowerMultiplierWithParamsSelector = mkSelector "readAttributeDcPowerMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcPowerDivisorWithParams:@
readAttributeDcPowerDivisorWithParamsSelector :: Selector
readAttributeDcPowerDivisorWithParamsSelector = mkSelector "readAttributeDcPowerDivisorWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyWithParams:@
readAttributeAcFrequencyWithParamsSelector :: Selector
readAttributeAcFrequencyWithParamsSelector = mkSelector "readAttributeAcFrequencyWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMinWithParams:@
readAttributeAcFrequencyMinWithParamsSelector :: Selector
readAttributeAcFrequencyMinWithParamsSelector = mkSelector "readAttributeAcFrequencyMinWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMaxWithParams:@
readAttributeAcFrequencyMaxWithParamsSelector :: Selector
readAttributeAcFrequencyMaxWithParamsSelector = mkSelector "readAttributeAcFrequencyMaxWithParams:"

-- | @Selector@ for @readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParamsSelector :: Selector
readAttributeNeutralCurrentWithParamsSelector = mkSelector "readAttributeNeutralCurrentWithParams:"

-- | @Selector@ for @readAttributeTotalActivePowerWithParams:@
readAttributeTotalActivePowerWithParamsSelector :: Selector
readAttributeTotalActivePowerWithParamsSelector = mkSelector "readAttributeTotalActivePowerWithParams:"

-- | @Selector@ for @readAttributeTotalReactivePowerWithParams:@
readAttributeTotalReactivePowerWithParamsSelector :: Selector
readAttributeTotalReactivePowerWithParamsSelector = mkSelector "readAttributeTotalReactivePowerWithParams:"

-- | @Selector@ for @readAttributeTotalApparentPowerWithParams:@
readAttributeTotalApparentPowerWithParamsSelector :: Selector
readAttributeTotalApparentPowerWithParamsSelector = mkSelector "readAttributeTotalApparentPowerWithParams:"

-- | @Selector@ for @readAttributeMeasured1stHarmonicCurrentWithParams:@
readAttributeMeasured1stHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured1stHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured1stHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured3rdHarmonicCurrentWithParams:@
readAttributeMeasured3rdHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured3rdHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured3rdHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured5thHarmonicCurrentWithParams:@
readAttributeMeasured5thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured5thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured5thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured7thHarmonicCurrentWithParams:@
readAttributeMeasured7thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured7thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured7thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured9thHarmonicCurrentWithParams:@
readAttributeMeasured9thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured9thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured9thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured11thHarmonicCurrentWithParams:@
readAttributeMeasured11thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasured11thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured11thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase1stHarmonicCurrentWithParams:@
readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase1stHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:@
readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase5thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase5thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase7thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase7thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase9thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase9thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase11thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector :: Selector
readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase11thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMultiplierWithParams:@
readAttributeAcFrequencyMultiplierWithParamsSelector :: Selector
readAttributeAcFrequencyMultiplierWithParamsSelector = mkSelector "readAttributeAcFrequencyMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyDivisorWithParams:@
readAttributeAcFrequencyDivisorWithParamsSelector :: Selector
readAttributeAcFrequencyDivisorWithParamsSelector = mkSelector "readAttributeAcFrequencyDivisorWithParams:"

-- | @Selector@ for @readAttributePowerMultiplierWithParams:@
readAttributePowerMultiplierWithParamsSelector :: Selector
readAttributePowerMultiplierWithParamsSelector = mkSelector "readAttributePowerMultiplierWithParams:"

-- | @Selector@ for @readAttributePowerDivisorWithParams:@
readAttributePowerDivisorWithParamsSelector :: Selector
readAttributePowerDivisorWithParamsSelector = mkSelector "readAttributePowerDivisorWithParams:"

-- | @Selector@ for @readAttributeHarmonicCurrentMultiplierWithParams:@
readAttributeHarmonicCurrentMultiplierWithParamsSelector :: Selector
readAttributeHarmonicCurrentMultiplierWithParamsSelector = mkSelector "readAttributeHarmonicCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributePhaseHarmonicCurrentMultiplierWithParams:@
readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector :: Selector
readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector = mkSelector "readAttributePhaseHarmonicCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeInstantaneousVoltageWithParams:@
readAttributeInstantaneousVoltageWithParamsSelector :: Selector
readAttributeInstantaneousVoltageWithParamsSelector = mkSelector "readAttributeInstantaneousVoltageWithParams:"

-- | @Selector@ for @readAttributeInstantaneousLineCurrentWithParams:@
readAttributeInstantaneousLineCurrentWithParamsSelector :: Selector
readAttributeInstantaneousLineCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousLineCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousActiveCurrentWithParams:@
readAttributeInstantaneousActiveCurrentWithParamsSelector :: Selector
readAttributeInstantaneousActiveCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousActiveCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousReactiveCurrentWithParams:@
readAttributeInstantaneousReactiveCurrentWithParamsSelector :: Selector
readAttributeInstantaneousReactiveCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousReactiveCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousPowerWithParams:@
readAttributeInstantaneousPowerWithParamsSelector :: Selector
readAttributeInstantaneousPowerWithParamsSelector = mkSelector "readAttributeInstantaneousPowerWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageWithParams:@
readAttributeRmsVoltageWithParamsSelector :: Selector
readAttributeRmsVoltageWithParamsSelector = mkSelector "readAttributeRmsVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinWithParams:@
readAttributeRmsVoltageMinWithParamsSelector :: Selector
readAttributeRmsVoltageMinWithParamsSelector = mkSelector "readAttributeRmsVoltageMinWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxWithParams:@
readAttributeRmsVoltageMaxWithParamsSelector :: Selector
readAttributeRmsVoltageMaxWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentWithParams:@
readAttributeRmsCurrentWithParamsSelector :: Selector
readAttributeRmsCurrentWithParamsSelector = mkSelector "readAttributeRmsCurrentWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinWithParams:@
readAttributeRmsCurrentMinWithParamsSelector :: Selector
readAttributeRmsCurrentMinWithParamsSelector = mkSelector "readAttributeRmsCurrentMinWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxWithParams:@
readAttributeRmsCurrentMaxWithParamsSelector :: Selector
readAttributeRmsCurrentMaxWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxWithParams:"

-- | @Selector@ for @readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParamsSelector :: Selector
readAttributeActivePowerWithParamsSelector = mkSelector "readAttributeActivePowerWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinWithParams:@
readAttributeActivePowerMinWithParamsSelector :: Selector
readAttributeActivePowerMinWithParamsSelector = mkSelector "readAttributeActivePowerMinWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxWithParams:@
readAttributeActivePowerMaxWithParamsSelector :: Selector
readAttributeActivePowerMaxWithParamsSelector = mkSelector "readAttributeActivePowerMaxWithParams:"

-- | @Selector@ for @readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParamsSelector :: Selector
readAttributeReactivePowerWithParamsSelector = mkSelector "readAttributeReactivePowerWithParams:"

-- | @Selector@ for @readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParamsSelector :: Selector
readAttributeApparentPowerWithParamsSelector = mkSelector "readAttributeApparentPowerWithParams:"

-- | @Selector@ for @readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParamsSelector :: Selector
readAttributePowerFactorWithParamsSelector = mkSelector "readAttributePowerFactorWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector :: Selector
readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodWithParams:"

-- | @Selector@ for @writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterWithParams:@
readAttributeAverageRmsUnderVoltageCounterWithParamsSelector :: Selector
readAttributeAverageRmsUnderVoltageCounterWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterWithParams:"

-- | @Selector@ for @writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodWithParams:@
readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector :: Selector
readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector :: Selector
readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodWithParams:@
readAttributeRmsVoltageSagPeriodWithParamsSelector :: Selector
readAttributeRmsVoltageSagPeriodWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodWithParams:@
readAttributeRmsVoltageSwellPeriodWithParamsSelector :: Selector
readAttributeRmsVoltageSwellPeriodWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAcVoltageMultiplierWithParams:@
readAttributeAcVoltageMultiplierWithParamsSelector :: Selector
readAttributeAcVoltageMultiplierWithParamsSelector = mkSelector "readAttributeAcVoltageMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcVoltageDivisorWithParams:@
readAttributeAcVoltageDivisorWithParamsSelector :: Selector
readAttributeAcVoltageDivisorWithParamsSelector = mkSelector "readAttributeAcVoltageDivisorWithParams:"

-- | @Selector@ for @readAttributeAcCurrentMultiplierWithParams:@
readAttributeAcCurrentMultiplierWithParamsSelector :: Selector
readAttributeAcCurrentMultiplierWithParamsSelector = mkSelector "readAttributeAcCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcCurrentDivisorWithParams:@
readAttributeAcCurrentDivisorWithParamsSelector :: Selector
readAttributeAcCurrentDivisorWithParamsSelector = mkSelector "readAttributeAcCurrentDivisorWithParams:"

-- | @Selector@ for @readAttributeAcPowerMultiplierWithParams:@
readAttributeAcPowerMultiplierWithParamsSelector :: Selector
readAttributeAcPowerMultiplierWithParamsSelector = mkSelector "readAttributeAcPowerMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcPowerDivisorWithParams:@
readAttributeAcPowerDivisorWithParamsSelector :: Selector
readAttributeAcPowerDivisorWithParamsSelector = mkSelector "readAttributeAcPowerDivisorWithParams:"

-- | @Selector@ for @readAttributeOverloadAlarmsMaskWithParams:@
readAttributeOverloadAlarmsMaskWithParamsSelector :: Selector
readAttributeOverloadAlarmsMaskWithParamsSelector = mkSelector "readAttributeOverloadAlarmsMaskWithParams:"

-- | @Selector@ for @writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeVoltageOverloadWithParams:@
readAttributeVoltageOverloadWithParamsSelector :: Selector
readAttributeVoltageOverloadWithParamsSelector = mkSelector "readAttributeVoltageOverloadWithParams:"

-- | @Selector@ for @readAttributeCurrentOverloadWithParams:@
readAttributeCurrentOverloadWithParamsSelector :: Selector
readAttributeCurrentOverloadWithParamsSelector = mkSelector "readAttributeCurrentOverloadWithParams:"

-- | @Selector@ for @readAttributeAcOverloadAlarmsMaskWithParams:@
readAttributeAcOverloadAlarmsMaskWithParamsSelector :: Selector
readAttributeAcOverloadAlarmsMaskWithParamsSelector = mkSelector "readAttributeAcOverloadAlarmsMaskWithParams:"

-- | @Selector@ for @writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAcVoltageOverloadWithParams:@
readAttributeAcVoltageOverloadWithParamsSelector :: Selector
readAttributeAcVoltageOverloadWithParamsSelector = mkSelector "readAttributeAcVoltageOverloadWithParams:"

-- | @Selector@ for @readAttributeAcCurrentOverloadWithParams:@
readAttributeAcCurrentOverloadWithParamsSelector :: Selector
readAttributeAcCurrentOverloadWithParamsSelector = mkSelector "readAttributeAcCurrentOverloadWithParams:"

-- | @Selector@ for @readAttributeAcActivePowerOverloadWithParams:@
readAttributeAcActivePowerOverloadWithParamsSelector :: Selector
readAttributeAcActivePowerOverloadWithParamsSelector = mkSelector "readAttributeAcActivePowerOverloadWithParams:"

-- | @Selector@ for @readAttributeAcReactivePowerOverloadWithParams:@
readAttributeAcReactivePowerOverloadWithParamsSelector :: Selector
readAttributeAcReactivePowerOverloadWithParamsSelector = mkSelector "readAttributeAcReactivePowerOverloadWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageWithParams:@
readAttributeAverageRmsOverVoltageWithParamsSelector :: Selector
readAttributeAverageRmsOverVoltageWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageWithParams:@
readAttributeAverageRmsUnderVoltageWithParamsSelector :: Selector
readAttributeAverageRmsUnderVoltageWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltageWithParams:@
readAttributeRmsExtremeOverVoltageWithParamsSelector :: Selector
readAttributeRmsExtremeOverVoltageWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltageWithParams:@
readAttributeRmsExtremeUnderVoltageWithParamsSelector :: Selector
readAttributeRmsExtremeUnderVoltageWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagWithParams:@
readAttributeRmsVoltageSagWithParamsSelector :: Selector
readAttributeRmsVoltageSagWithParamsSelector = mkSelector "readAttributeRmsVoltageSagWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellWithParams:@
readAttributeRmsVoltageSwellWithParamsSelector :: Selector
readAttributeRmsVoltageSwellWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellWithParams:"

-- | @Selector@ for @readAttributeLineCurrentPhaseBWithParams:@
readAttributeLineCurrentPhaseBWithParamsSelector :: Selector
readAttributeLineCurrentPhaseBWithParamsSelector = mkSelector "readAttributeLineCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentPhaseBWithParams:@
readAttributeActiveCurrentPhaseBWithParamsSelector :: Selector
readAttributeActiveCurrentPhaseBWithParamsSelector = mkSelector "readAttributeActiveCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentPhaseBWithParams:@
readAttributeReactiveCurrentPhaseBWithParamsSelector :: Selector
readAttributeReactiveCurrentPhaseBWithParamsSelector = mkSelector "readAttributeReactiveCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltagePhaseBWithParams:@
readAttributeRmsVoltagePhaseBWithParamsSelector :: Selector
readAttributeRmsVoltagePhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltagePhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinPhaseBWithParams:@
readAttributeRmsVoltageMinPhaseBWithParamsSelector :: Selector
readAttributeRmsVoltageMinPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxPhaseBWithParams:@
readAttributeRmsVoltageMaxPhaseBWithParamsSelector :: Selector
readAttributeRmsVoltageMaxPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentPhaseBWithParams:@
readAttributeRmsCurrentPhaseBWithParamsSelector :: Selector
readAttributeRmsCurrentPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinPhaseBWithParams:@
readAttributeRmsCurrentMinPhaseBWithParamsSelector :: Selector
readAttributeRmsCurrentMinPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxPhaseBWithParams:@
readAttributeRmsCurrentMaxPhaseBWithParamsSelector :: Selector
readAttributeRmsCurrentMaxPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerPhaseBWithParams:@
readAttributeActivePowerPhaseBWithParamsSelector :: Selector
readAttributeActivePowerPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinPhaseBWithParams:@
readAttributeActivePowerMinPhaseBWithParamsSelector :: Selector
readAttributeActivePowerMinPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxPhaseBWithParams:@
readAttributeActivePowerMaxPhaseBWithParamsSelector :: Selector
readAttributeActivePowerMaxPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeReactivePowerPhaseBWithParams:@
readAttributeReactivePowerPhaseBWithParamsSelector :: Selector
readAttributeReactivePowerPhaseBWithParamsSelector = mkSelector "readAttributeReactivePowerPhaseBWithParams:"

-- | @Selector@ for @readAttributeApparentPowerPhaseBWithParams:@
readAttributeApparentPowerPhaseBWithParamsSelector :: Selector
readAttributeApparentPowerPhaseBWithParamsSelector = mkSelector "readAttributeApparentPowerPhaseBWithParams:"

-- | @Selector@ for @readAttributePowerFactorPhaseBWithParams:@
readAttributePowerFactorPhaseBWithParamsSelector :: Selector
readAttributePowerFactorPhaseBWithParamsSelector = mkSelector "readAttributePowerFactorPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector :: Selector
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector :: Selector
readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector :: Selector
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector :: Selector
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector :: Selector
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodPhaseBWithParams:@
readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector :: Selector
readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodPhaseBWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector :: Selector
readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeLineCurrentPhaseCWithParams:@
readAttributeLineCurrentPhaseCWithParamsSelector :: Selector
readAttributeLineCurrentPhaseCWithParamsSelector = mkSelector "readAttributeLineCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentPhaseCWithParams:@
readAttributeActiveCurrentPhaseCWithParamsSelector :: Selector
readAttributeActiveCurrentPhaseCWithParamsSelector = mkSelector "readAttributeActiveCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentPhaseCWithParams:@
readAttributeReactiveCurrentPhaseCWithParamsSelector :: Selector
readAttributeReactiveCurrentPhaseCWithParamsSelector = mkSelector "readAttributeReactiveCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltagePhaseCWithParams:@
readAttributeRmsVoltagePhaseCWithParamsSelector :: Selector
readAttributeRmsVoltagePhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltagePhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinPhaseCWithParams:@
readAttributeRmsVoltageMinPhaseCWithParamsSelector :: Selector
readAttributeRmsVoltageMinPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxPhaseCWithParams:@
readAttributeRmsVoltageMaxPhaseCWithParamsSelector :: Selector
readAttributeRmsVoltageMaxPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentPhaseCWithParams:@
readAttributeRmsCurrentPhaseCWithParamsSelector :: Selector
readAttributeRmsCurrentPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinPhaseCWithParams:@
readAttributeRmsCurrentMinPhaseCWithParamsSelector :: Selector
readAttributeRmsCurrentMinPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxPhaseCWithParams:@
readAttributeRmsCurrentMaxPhaseCWithParamsSelector :: Selector
readAttributeRmsCurrentMaxPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerPhaseCWithParams:@
readAttributeActivePowerPhaseCWithParamsSelector :: Selector
readAttributeActivePowerPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinPhaseCWithParams:@
readAttributeActivePowerMinPhaseCWithParamsSelector :: Selector
readAttributeActivePowerMinPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxPhaseCWithParams:@
readAttributeActivePowerMaxPhaseCWithParamsSelector :: Selector
readAttributeActivePowerMaxPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeReactivePowerPhaseCWithParams:@
readAttributeReactivePowerPhaseCWithParamsSelector :: Selector
readAttributeReactivePowerPhaseCWithParamsSelector = mkSelector "readAttributeReactivePowerPhaseCWithParams:"

-- | @Selector@ for @readAttributeApparentPowerPhaseCWithParams:@
readAttributeApparentPowerPhaseCWithParamsSelector :: Selector
readAttributeApparentPowerPhaseCWithParamsSelector = mkSelector "readAttributeApparentPowerPhaseCWithParams:"

-- | @Selector@ for @readAttributePowerFactorPhaseCWithParams:@
readAttributePowerFactorPhaseCWithParamsSelector :: Selector
readAttributePowerFactorPhaseCWithParamsSelector = mkSelector "readAttributePowerFactorPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector :: Selector
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector :: Selector
readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector :: Selector
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector :: Selector
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector :: Selector
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodPhaseCWithParams:@
readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector :: Selector
readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodPhaseCWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector :: Selector
readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodPhaseCWithParams:"

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

-- | @Selector@ for @getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

