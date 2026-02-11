{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Door Lock    An interface to a generic way to secure a door
--
-- Generated bindings for @MTRClusterDoorLock@.
module ObjC.Matter.MTRClusterDoorLock
  ( MTRClusterDoorLock
  , IsMTRClusterDoorLock(..)
  , lockDoorWithParams_expectedValues_expectedValueInterval_completion
  , lockDoorWithExpectedValues_expectedValueInterval_completion
  , unlockDoorWithParams_expectedValues_expectedValueInterval_completion
  , unlockDoorWithExpectedValues_expectedValueInterval_completion
  , unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completion
  , setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion
  , setUserWithParams_expectedValues_expectedValueInterval_completion
  , getUserWithParams_expectedValues_expectedValueInterval_completion
  , clearUserWithParams_expectedValues_expectedValueInterval_completion
  , setCredentialWithParams_expectedValues_expectedValueInterval_completion
  , getCredentialStatusWithParams_expectedValues_expectedValueInterval_completion
  , clearCredentialWithParams_expectedValues_expectedValueInterval_completion
  , unboltDoorWithParams_expectedValues_expectedValueInterval_completion
  , unboltDoorWithExpectedValues_expectedValueInterval_completion
  , setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion
  , clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion
  , clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion
  , appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completion
  , appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completion
  , appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completion
  , appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion
  , appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion
  , appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion
  , readAttributeLockStateWithParams
  , readAttributeLockTypeWithParams
  , readAttributeActuatorEnabledWithParams
  , readAttributeDoorStateWithParams
  , readAttributeDoorOpenEventsWithParams
  , writeAttributeDoorOpenEventsWithValue_expectedValueInterval
  , writeAttributeDoorOpenEventsWithValue_expectedValueInterval_params
  , readAttributeDoorClosedEventsWithParams
  , writeAttributeDoorClosedEventsWithValue_expectedValueInterval
  , writeAttributeDoorClosedEventsWithValue_expectedValueInterval_params
  , readAttributeOpenPeriodWithParams
  , writeAttributeOpenPeriodWithValue_expectedValueInterval
  , writeAttributeOpenPeriodWithValue_expectedValueInterval_params
  , readAttributeNumberOfTotalUsersSupportedWithParams
  , readAttributeNumberOfPINUsersSupportedWithParams
  , readAttributeNumberOfRFIDUsersSupportedWithParams
  , readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams
  , readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams
  , readAttributeNumberOfHolidaySchedulesSupportedWithParams
  , readAttributeMaxPINCodeLengthWithParams
  , readAttributeMinPINCodeLengthWithParams
  , readAttributeMaxRFIDCodeLengthWithParams
  , readAttributeMinRFIDCodeLengthWithParams
  , readAttributeCredentialRulesSupportWithParams
  , readAttributeNumberOfCredentialsSupportedPerUserWithParams
  , readAttributeLanguageWithParams
  , writeAttributeLanguageWithValue_expectedValueInterval
  , writeAttributeLanguageWithValue_expectedValueInterval_params
  , readAttributeLEDSettingsWithParams
  , writeAttributeLEDSettingsWithValue_expectedValueInterval
  , writeAttributeLEDSettingsWithValue_expectedValueInterval_params
  , readAttributeAutoRelockTimeWithParams
  , writeAttributeAutoRelockTimeWithValue_expectedValueInterval
  , writeAttributeAutoRelockTimeWithValue_expectedValueInterval_params
  , readAttributeSoundVolumeWithParams
  , writeAttributeSoundVolumeWithValue_expectedValueInterval
  , writeAttributeSoundVolumeWithValue_expectedValueInterval_params
  , readAttributeOperatingModeWithParams
  , writeAttributeOperatingModeWithValue_expectedValueInterval
  , writeAttributeOperatingModeWithValue_expectedValueInterval_params
  , readAttributeSupportedOperatingModesWithParams
  , readAttributeDefaultConfigurationRegisterWithParams
  , readAttributeEnableLocalProgrammingWithParams
  , writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval
  , writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_params
  , readAttributeEnableOneTouchLockingWithParams
  , writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval
  , writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_params
  , readAttributeEnableInsideStatusLEDWithParams
  , writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval
  , writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_params
  , readAttributeEnablePrivacyModeButtonWithParams
  , writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval
  , writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_params
  , readAttributeLocalProgrammingFeaturesWithParams
  , writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval
  , writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_params
  , readAttributeWrongCodeEntryLimitWithParams
  , writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval
  , writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_params
  , readAttributeUserCodeTemporaryDisableTimeWithParams
  , writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval
  , writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_params
  , readAttributeSendPINOverTheAirWithParams
  , writeAttributeSendPINOverTheAirWithValue_expectedValueInterval
  , writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_params
  , readAttributeRequirePINforRemoteOperationWithParams
  , writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval
  , writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_params
  , readAttributeExpiringUserTimeoutWithParams
  , writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval
  , writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_params
  , readAttributeAliroReaderVerificationKeyWithParams
  , readAttributeAliroReaderGroupIdentifierWithParams
  , readAttributeAliroReaderGroupSubIdentifierWithParams
  , readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams
  , readAttributeAliroGroupResolvingKeyWithParams
  , readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams
  , readAttributeAliroBLEAdvertisingVersionWithParams
  , readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams
  , readAttributeNumberOfAliroEndpointKeysSupportedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , readAttributeAppleAliroReaderVerificationKeyWithParams
  , readAttributeAppleAliroReaderGroupIdentifierWithParams
  , readAttributeAppleAliroReaderGroupSubIdentifierWithParams
  , readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams
  , readAttributeAppleAliroGroupResolvingKeyWithParams
  , readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams
  , readAttributeAppleAliroBLEAdvertisingVersionWithParams
  , readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams
  , readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , lockDoorWithParams_expectedValues_expectedValueInterval_completionHandler
  , unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandler
  , unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandler
  , setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , setUserWithParams_expectedValues_expectedValueInterval_completionHandler
  , getUserWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearUserWithParams_expectedValues_expectedValueInterval_completionHandler
  , setCredentialWithParams_expectedValues_expectedValueInterval_completionHandler
  , getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , lockDoorWithParams_expectedValues_expectedValueInterval_completionSelector
  , lockDoorWithExpectedValues_expectedValueInterval_completionSelector
  , unlockDoorWithParams_expectedValues_expectedValueInterval_completionSelector
  , unlockDoorWithExpectedValues_expectedValueInterval_completionSelector
  , unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionSelector
  , setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , setUserWithParams_expectedValues_expectedValueInterval_completionSelector
  , getUserWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearUserWithParams_expectedValues_expectedValueInterval_completionSelector
  , setCredentialWithParams_expectedValues_expectedValueInterval_completionSelector
  , getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearCredentialWithParams_expectedValues_expectedValueInterval_completionSelector
  , unboltDoorWithParams_expectedValues_expectedValueInterval_completionSelector
  , unboltDoorWithExpectedValues_expectedValueInterval_completionSelector
  , setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector
  , appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector
  , appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector
  , appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeLockStateWithParamsSelector
  , readAttributeLockTypeWithParamsSelector
  , readAttributeActuatorEnabledWithParamsSelector
  , readAttributeDoorStateWithParamsSelector
  , readAttributeDoorOpenEventsWithParamsSelector
  , writeAttributeDoorOpenEventsWithValue_expectedValueIntervalSelector
  , writeAttributeDoorOpenEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeDoorClosedEventsWithParamsSelector
  , writeAttributeDoorClosedEventsWithValue_expectedValueIntervalSelector
  , writeAttributeDoorClosedEventsWithValue_expectedValueInterval_paramsSelector
  , readAttributeOpenPeriodWithParamsSelector
  , writeAttributeOpenPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeOpenPeriodWithValue_expectedValueInterval_paramsSelector
  , readAttributeNumberOfTotalUsersSupportedWithParamsSelector
  , readAttributeNumberOfPINUsersSupportedWithParamsSelector
  , readAttributeNumberOfRFIDUsersSupportedWithParamsSelector
  , readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParamsSelector
  , readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParamsSelector
  , readAttributeNumberOfHolidaySchedulesSupportedWithParamsSelector
  , readAttributeMaxPINCodeLengthWithParamsSelector
  , readAttributeMinPINCodeLengthWithParamsSelector
  , readAttributeMaxRFIDCodeLengthWithParamsSelector
  , readAttributeMinRFIDCodeLengthWithParamsSelector
  , readAttributeCredentialRulesSupportWithParamsSelector
  , readAttributeNumberOfCredentialsSupportedPerUserWithParamsSelector
  , readAttributeLanguageWithParamsSelector
  , writeAttributeLanguageWithValue_expectedValueIntervalSelector
  , writeAttributeLanguageWithValue_expectedValueInterval_paramsSelector
  , readAttributeLEDSettingsWithParamsSelector
  , writeAttributeLEDSettingsWithValue_expectedValueIntervalSelector
  , writeAttributeLEDSettingsWithValue_expectedValueInterval_paramsSelector
  , readAttributeAutoRelockTimeWithParamsSelector
  , writeAttributeAutoRelockTimeWithValue_expectedValueIntervalSelector
  , writeAttributeAutoRelockTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeSoundVolumeWithParamsSelector
  , writeAttributeSoundVolumeWithValue_expectedValueIntervalSelector
  , writeAttributeSoundVolumeWithValue_expectedValueInterval_paramsSelector
  , readAttributeOperatingModeWithParamsSelector
  , writeAttributeOperatingModeWithValue_expectedValueIntervalSelector
  , writeAttributeOperatingModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedOperatingModesWithParamsSelector
  , readAttributeDefaultConfigurationRegisterWithParamsSelector
  , readAttributeEnableLocalProgrammingWithParamsSelector
  , writeAttributeEnableLocalProgrammingWithValue_expectedValueIntervalSelector
  , writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_paramsSelector
  , readAttributeEnableOneTouchLockingWithParamsSelector
  , writeAttributeEnableOneTouchLockingWithValue_expectedValueIntervalSelector
  , writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_paramsSelector
  , readAttributeEnableInsideStatusLEDWithParamsSelector
  , writeAttributeEnableInsideStatusLEDWithValue_expectedValueIntervalSelector
  , writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_paramsSelector
  , readAttributeEnablePrivacyModeButtonWithParamsSelector
  , writeAttributeEnablePrivacyModeButtonWithValue_expectedValueIntervalSelector
  , writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_paramsSelector
  , readAttributeLocalProgrammingFeaturesWithParamsSelector
  , writeAttributeLocalProgrammingFeaturesWithValue_expectedValueIntervalSelector
  , writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_paramsSelector
  , readAttributeWrongCodeEntryLimitWithParamsSelector
  , writeAttributeWrongCodeEntryLimitWithValue_expectedValueIntervalSelector
  , writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_paramsSelector
  , readAttributeUserCodeTemporaryDisableTimeWithParamsSelector
  , writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueIntervalSelector
  , writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeSendPINOverTheAirWithParamsSelector
  , writeAttributeSendPINOverTheAirWithValue_expectedValueIntervalSelector
  , writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_paramsSelector
  , readAttributeRequirePINforRemoteOperationWithParamsSelector
  , writeAttributeRequirePINforRemoteOperationWithValue_expectedValueIntervalSelector
  , writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_paramsSelector
  , readAttributeExpiringUserTimeoutWithParamsSelector
  , writeAttributeExpiringUserTimeoutWithValue_expectedValueIntervalSelector
  , writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_paramsSelector
  , readAttributeAliroReaderVerificationKeyWithParamsSelector
  , readAttributeAliroReaderGroupIdentifierWithParamsSelector
  , readAttributeAliroReaderGroupSubIdentifierWithParamsSelector
  , readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector
  , readAttributeAliroGroupResolvingKeyWithParamsSelector
  , readAttributeAliroSupportedBLEUWBProtocolVersionsWithParamsSelector
  , readAttributeAliroBLEAdvertisingVersionWithParamsSelector
  , readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector
  , readAttributeNumberOfAliroEndpointKeysSupportedWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeAppleAliroReaderVerificationKeyWithParamsSelector
  , readAttributeAppleAliroReaderGroupIdentifierWithParamsSelector
  , readAttributeAppleAliroReaderGroupSubIdentifierWithParamsSelector
  , readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector
  , readAttributeAppleAliroGroupResolvingKeyWithParamsSelector
  , readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParamsSelector
  , readAttributeAppleAliroBLEAdvertisingVersionWithParamsSelector
  , readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector
  , readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , lockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- lockDoorWithParams:expectedValues:expectedValueInterval:completion:@
lockDoorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterLockDoorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lockDoorWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "lockDoorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- lockDoorWithExpectedValues:expectedValueInterval:completion:@
lockDoorWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
lockDoorWithExpectedValues_expectedValueInterval_completion mtrClusterDoorLock  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "lockDoorWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unlockDoorWithParams:expectedValues:expectedValueInterval:completion:@
unlockDoorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterUnlockDoorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unlockDoorWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "unlockDoorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unlockDoorWithExpectedValues:expectedValueInterval:completion:@
unlockDoorWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
unlockDoorWithExpectedValues_expectedValueInterval_completion mtrClusterDoorLock  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "unlockDoorWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completion:@
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterUnlockWithTimeoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setUserWithParams:expectedValues:expectedValueInterval:completion:@
setUserWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setUserWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setUserWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getUserWithParams:expectedValues:expectedValueInterval:completion:@
getUserWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getUserWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getUserWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearUserWithParams:expectedValues:expectedValueInterval:completion:@
clearUserWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearUserWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearUserWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setCredentialWithParams:expectedValues:expectedValueInterval:completion:@
setCredentialWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setCredentialWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setCredentialWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:@
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetCredentialStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearCredentialWithParams:expectedValues:expectedValueInterval:completion:@
clearCredentialWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearCredentialWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearCredentialWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unboltDoorWithParams:expectedValues:expectedValueInterval:completion:@
unboltDoorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterUnboltDoorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unboltDoorWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "unboltDoorWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unboltDoorWithExpectedValues:expectedValueInterval:completion:@
unboltDoorWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
unboltDoorWithExpectedValues_expectedValueInterval_completion mtrClusterDoorLock  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "unboltDoorWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetAliroReaderConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearAliroReaderConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:@
clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion mtrClusterDoorLock  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "clearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleSetAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:@
appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterAppleSetAliroCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "appleSetAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleGetAliroCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:@
appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterAppleGetAliroCredentialStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "appleGetAliroCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleClearAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:@
appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterAppleClearAliroCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "appleClearAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleSetAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterAppleSetAliroReaderConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "appleSetAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleClearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterAppleClearAliroReaderConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "appleClearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- appleClearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:@
appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completion mtrClusterDoorLock  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "appleClearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLockStateWithParams:@
readAttributeLockStateWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeLockStateWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeLockStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLockTypeWithParams:@
readAttributeLockTypeWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeLockTypeWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeLockTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActuatorEnabledWithParams:@
readAttributeActuatorEnabledWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeActuatorEnabledWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeActuatorEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDoorStateWithParams:@
readAttributeDoorStateWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeDoorStateWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeDoorStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDoorOpenEventsWithParams:@
readAttributeDoorOpenEventsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeDoorOpenEventsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeDoorOpenEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDoorOpenEventsWithValue:expectedValueInterval:@
writeAttributeDoorOpenEventsWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDoorOpenEventsWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeDoorOpenEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDoorOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeDoorOpenEventsWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDoorOpenEventsWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeDoorOpenEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeDoorClosedEventsWithParams:@
readAttributeDoorClosedEventsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeDoorClosedEventsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeDoorClosedEventsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDoorClosedEventsWithValue:expectedValueInterval:@
writeAttributeDoorClosedEventsWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDoorClosedEventsWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeDoorClosedEventsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDoorClosedEventsWithValue:expectedValueInterval:params:@
writeAttributeDoorClosedEventsWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDoorClosedEventsWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeDoorClosedEventsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOpenPeriodWithParams:@
readAttributeOpenPeriodWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeOpenPeriodWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeOpenPeriodWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOpenPeriodWithValue:expectedValueInterval:@
writeAttributeOpenPeriodWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOpenPeriodWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeOpenPeriodWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeOpenPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOpenPeriodWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeOpenPeriodWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeNumberOfTotalUsersSupportedWithParams:@
readAttributeNumberOfTotalUsersSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfTotalUsersSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfTotalUsersSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfPINUsersSupportedWithParams:@
readAttributeNumberOfPINUsersSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfPINUsersSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfPINUsersSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfRFIDUsersSupportedWithParams:@
readAttributeNumberOfRFIDUsersSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfRFIDUsersSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfRFIDUsersSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams:@
readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams:@
readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfHolidaySchedulesSupportedWithParams:@
readAttributeNumberOfHolidaySchedulesSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfHolidaySchedulesSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfHolidaySchedulesSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxPINCodeLengthWithParams:@
readAttributeMaxPINCodeLengthWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeMaxPINCodeLengthWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeMaxPINCodeLengthWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinPINCodeLengthWithParams:@
readAttributeMinPINCodeLengthWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeMinPINCodeLengthWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeMinPINCodeLengthWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxRFIDCodeLengthWithParams:@
readAttributeMaxRFIDCodeLengthWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeMaxRFIDCodeLengthWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeMaxRFIDCodeLengthWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinRFIDCodeLengthWithParams:@
readAttributeMinRFIDCodeLengthWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeMinRFIDCodeLengthWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeMinRFIDCodeLengthWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCredentialRulesSupportWithParams:@
readAttributeCredentialRulesSupportWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeCredentialRulesSupportWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeCredentialRulesSupportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfCredentialsSupportedPerUserWithParams:@
readAttributeNumberOfCredentialsSupportedPerUserWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfCredentialsSupportedPerUserWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfCredentialsSupportedPerUserWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLanguageWithParams:@
readAttributeLanguageWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeLanguageWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeLanguageWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLanguageWithValue:expectedValueInterval:@
writeAttributeLanguageWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLanguageWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLanguageWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLanguageWithValue:expectedValueInterval:params:@
writeAttributeLanguageWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLanguageWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLanguageWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLEDSettingsWithParams:@
readAttributeLEDSettingsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeLEDSettingsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeLEDSettingsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLEDSettingsWithValue:expectedValueInterval:@
writeAttributeLEDSettingsWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLEDSettingsWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLEDSettingsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLEDSettingsWithValue:expectedValueInterval:params:@
writeAttributeLEDSettingsWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLEDSettingsWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLEDSettingsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAutoRelockTimeWithParams:@
readAttributeAutoRelockTimeWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAutoRelockTimeWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAutoRelockTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAutoRelockTimeWithValue:expectedValueInterval:@
writeAttributeAutoRelockTimeWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAutoRelockTimeWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeAutoRelockTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAutoRelockTimeWithValue:expectedValueInterval:params:@
writeAttributeAutoRelockTimeWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAutoRelockTimeWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeAutoRelockTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSoundVolumeWithParams:@
readAttributeSoundVolumeWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeSoundVolumeWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeSoundVolumeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSoundVolumeWithValue:expectedValueInterval:@
writeAttributeSoundVolumeWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSoundVolumeWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeSoundVolumeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSoundVolumeWithValue:expectedValueInterval:params:@
writeAttributeSoundVolumeWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSoundVolumeWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeSoundVolumeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeOperatingModeWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeOperatingModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOperatingModeWithValue:expectedValueInterval:@
writeAttributeOperatingModeWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOperatingModeWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeOperatingModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOperatingModeWithValue:expectedValueInterval:params:@
writeAttributeOperatingModeWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOperatingModeWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeOperatingModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedOperatingModesWithParams:@
readAttributeSupportedOperatingModesWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeSupportedOperatingModesWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeSupportedOperatingModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultConfigurationRegisterWithParams:@
readAttributeDefaultConfigurationRegisterWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeDefaultConfigurationRegisterWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeDefaultConfigurationRegisterWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEnableLocalProgrammingWithParams:@
readAttributeEnableLocalProgrammingWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeEnableLocalProgrammingWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeEnableLocalProgrammingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:@
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:params:@
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeEnableOneTouchLockingWithParams:@
readAttributeEnableOneTouchLockingWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeEnableOneTouchLockingWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeEnableOneTouchLockingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:@
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:params:@
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeEnableInsideStatusLEDWithParams:@
readAttributeEnableInsideStatusLEDWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeEnableInsideStatusLEDWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeEnableInsideStatusLEDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:@
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:params:@
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeEnablePrivacyModeButtonWithParams:@
readAttributeEnablePrivacyModeButtonWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeEnablePrivacyModeButtonWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeEnablePrivacyModeButtonWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:@
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:params:@
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLocalProgrammingFeaturesWithParams:@
readAttributeLocalProgrammingFeaturesWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeLocalProgrammingFeaturesWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeLocalProgrammingFeaturesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:@
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:params:@
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeWrongCodeEntryLimitWithParams:@
readAttributeWrongCodeEntryLimitWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeWrongCodeEntryLimitWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeWrongCodeEntryLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:@
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:params:@
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUserCodeTemporaryDisableTimeWithParams:@
readAttributeUserCodeTemporaryDisableTimeWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeUserCodeTemporaryDisableTimeWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeUserCodeTemporaryDisableTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:@
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:params:@
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSendPINOverTheAirWithParams:@
readAttributeSendPINOverTheAirWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeSendPINOverTheAirWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeSendPINOverTheAirWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:@
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:params:@
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRequirePINforRemoteOperationWithParams:@
readAttributeRequirePINforRemoteOperationWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeRequirePINforRemoteOperationWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeRequirePINforRemoteOperationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:@
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:params:@
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeExpiringUserTimeoutWithParams:@
readAttributeExpiringUserTimeoutWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeExpiringUserTimeoutWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeExpiringUserTimeoutWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:@
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDoorLock (mkSelector "writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:params:@
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_params :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterDoorLock -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_params mtrClusterDoorLock  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterDoorLock (mkSelector "writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeAliroReaderVerificationKeyWithParams:@
readAttributeAliroReaderVerificationKeyWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroReaderVerificationKeyWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroReaderVerificationKeyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroReaderGroupIdentifierWithParams:@
readAttributeAliroReaderGroupIdentifierWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroReaderGroupIdentifierWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroReaderGroupIdentifierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroReaderGroupSubIdentifierWithParams:@
readAttributeAliroReaderGroupSubIdentifierWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroReaderGroupSubIdentifierWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroReaderGroupSubIdentifierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams:@
readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroGroupResolvingKeyWithParams:@
readAttributeAliroGroupResolvingKeyWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroGroupResolvingKeyWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroGroupResolvingKeyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams:@
readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAliroBLEAdvertisingVersionWithParams:@
readAttributeAliroBLEAdvertisingVersionWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAliroBLEAdvertisingVersionWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAliroBLEAdvertisingVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams:@
readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfAliroEndpointKeysSupportedWithParams:@
readAttributeNumberOfAliroEndpointKeysSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeNumberOfAliroEndpointKeysSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeNumberOfAliroEndpointKeysSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroReaderVerificationKeyWithParams:@
readAttributeAppleAliroReaderVerificationKeyWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroReaderVerificationKeyWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroReaderVerificationKeyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroReaderGroupIdentifierWithParams:@
readAttributeAppleAliroReaderGroupIdentifierWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroReaderGroupIdentifierWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroReaderGroupIdentifierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroReaderGroupSubIdentifierWithParams:@
readAttributeAppleAliroReaderGroupSubIdentifierWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroReaderGroupSubIdentifierWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroReaderGroupSubIdentifierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams:@
readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroGroupResolvingKeyWithParams:@
readAttributeAppleAliroGroupResolvingKeyWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroGroupResolvingKeyWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroGroupResolvingKeyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams:@
readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleAliroBLEAdvertisingVersionWithParams:@
readAttributeAppleAliroBLEAdvertisingVersionWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleAliroBLEAdvertisingVersionWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleAliroBLEAdvertisingVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams:@
readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams:@
readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRReadParams params) => mtrClusterDoorLock -> params -> IO (Id NSDictionary)
readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams mtrClusterDoorLock  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDoorLock (mkSelector "readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterDoorLock mtrClusterDoorLock => mtrClusterDoorLock -> IO (Id MTRClusterDoorLock)
init_ mtrClusterDoorLock  =
    sendMsg mtrClusterDoorLock (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterDoorLock)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDoorLock"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDevice device, IsNSObject queue) => mtrClusterDoorLock -> device -> CUShort -> queue -> IO (Id MTRClusterDoorLock)
initWithDevice_endpoint_queue mtrClusterDoorLock  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterDoorLock (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- lockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:@
lockDoorWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterLockDoorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lockDoorWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "lockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- unlockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:@
unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterUnlockDoorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "unlockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterUnlockWithTimeoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearWeekDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearYearDayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearHolidayScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
setUserWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setUserWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setUserWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
getUserWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getUserWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getUserWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearUserWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearUserParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearUserWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearUserWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:@
setCredentialWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterSetCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setCredentialWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "setCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getCredentialStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterGetCredentialStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "getCredentialStatusWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDoorLockClusterClearCredentialParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDoorLock -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDoorLock  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDoorLock (mkSelector "clearCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDoorLock mtrClusterDoorLock, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDoorLock -> device -> endpointID -> queue -> IO (Id MTRClusterDoorLock)
initWithDevice_endpointID_queue mtrClusterDoorLock  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterDoorLock (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockDoorWithParams:expectedValues:expectedValueInterval:completion:@
lockDoorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
lockDoorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "lockDoorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @lockDoorWithExpectedValues:expectedValueInterval:completion:@
lockDoorWithExpectedValues_expectedValueInterval_completionSelector :: Selector
lockDoorWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "lockDoorWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unlockDoorWithParams:expectedValues:expectedValueInterval:completion:@
unlockDoorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
unlockDoorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unlockDoorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unlockDoorWithExpectedValues:expectedValueInterval:completion:@
unlockDoorWithExpectedValues_expectedValueInterval_completionSelector :: Selector
unlockDoorWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "unlockDoorWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completion:@
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setUserWithParams:expectedValues:expectedValueInterval:completion:@
setUserWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setUserWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setUserWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getUserWithParams:expectedValues:expectedValueInterval:completion:@
getUserWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getUserWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getUserWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearUserWithParams:expectedValues:expectedValueInterval:completion:@
clearUserWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearUserWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearUserWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setCredentialWithParams:expectedValues:expectedValueInterval:completion:@
setCredentialWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setCredentialWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setCredentialWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:@
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearCredentialWithParams:expectedValues:expectedValueInterval:completion:@
clearCredentialWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearCredentialWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearCredentialWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unboltDoorWithParams:expectedValues:expectedValueInterval:completion:@
unboltDoorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
unboltDoorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unboltDoorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unboltDoorWithExpectedValues:expectedValueInterval:completion:@
unboltDoorWithExpectedValues_expectedValueInterval_completionSelector :: Selector
unboltDoorWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "unboltDoorWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:@
clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector :: Selector
clearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "clearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleSetAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:@
appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
appleSetAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "appleSetAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleGetAliroCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:@
appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
appleGetAliroCredentialStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "appleGetAliroCredentialStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleClearAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:@
appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
appleClearAliroCredentialWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "appleClearAliroCredentialWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleSetAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
appleSetAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "appleSetAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleClearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:@
appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
appleClearAliroReaderConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "appleClearAliroReaderConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @appleClearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:@
appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector :: Selector
appleClearAliroReaderConfigWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "appleClearAliroReaderConfigWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeLockStateWithParams:@
readAttributeLockStateWithParamsSelector :: Selector
readAttributeLockStateWithParamsSelector = mkSelector "readAttributeLockStateWithParams:"

-- | @Selector@ for @readAttributeLockTypeWithParams:@
readAttributeLockTypeWithParamsSelector :: Selector
readAttributeLockTypeWithParamsSelector = mkSelector "readAttributeLockTypeWithParams:"

-- | @Selector@ for @readAttributeActuatorEnabledWithParams:@
readAttributeActuatorEnabledWithParamsSelector :: Selector
readAttributeActuatorEnabledWithParamsSelector = mkSelector "readAttributeActuatorEnabledWithParams:"

-- | @Selector@ for @readAttributeDoorStateWithParams:@
readAttributeDoorStateWithParamsSelector :: Selector
readAttributeDoorStateWithParamsSelector = mkSelector "readAttributeDoorStateWithParams:"

-- | @Selector@ for @readAttributeDoorOpenEventsWithParams:@
readAttributeDoorOpenEventsWithParamsSelector :: Selector
readAttributeDoorOpenEventsWithParamsSelector = mkSelector "readAttributeDoorOpenEventsWithParams:"

-- | @Selector@ for @writeAttributeDoorOpenEventsWithValue:expectedValueInterval:@
writeAttributeDoorOpenEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDoorOpenEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDoorOpenEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDoorOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeDoorOpenEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDoorOpenEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDoorOpenEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeDoorClosedEventsWithParams:@
readAttributeDoorClosedEventsWithParamsSelector :: Selector
readAttributeDoorClosedEventsWithParamsSelector = mkSelector "readAttributeDoorClosedEventsWithParams:"

-- | @Selector@ for @writeAttributeDoorClosedEventsWithValue:expectedValueInterval:@
writeAttributeDoorClosedEventsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDoorClosedEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDoorClosedEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDoorClosedEventsWithValue:expectedValueInterval:params:@
writeAttributeDoorClosedEventsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDoorClosedEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDoorClosedEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOpenPeriodWithParams:@
readAttributeOpenPeriodWithParamsSelector :: Selector
readAttributeOpenPeriodWithParamsSelector = mkSelector "readAttributeOpenPeriodWithParams:"

-- | @Selector@ for @writeAttributeOpenPeriodWithValue:expectedValueInterval:@
writeAttributeOpenPeriodWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOpenPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOpenPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeOpenPeriodWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOpenPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOpenPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNumberOfTotalUsersSupportedWithParams:@
readAttributeNumberOfTotalUsersSupportedWithParamsSelector :: Selector
readAttributeNumberOfTotalUsersSupportedWithParamsSelector = mkSelector "readAttributeNumberOfTotalUsersSupportedWithParams:"

-- | @Selector@ for @readAttributeNumberOfPINUsersSupportedWithParams:@
readAttributeNumberOfPINUsersSupportedWithParamsSelector :: Selector
readAttributeNumberOfPINUsersSupportedWithParamsSelector = mkSelector "readAttributeNumberOfPINUsersSupportedWithParams:"

-- | @Selector@ for @readAttributeNumberOfRFIDUsersSupportedWithParams:@
readAttributeNumberOfRFIDUsersSupportedWithParamsSelector :: Selector
readAttributeNumberOfRFIDUsersSupportedWithParamsSelector = mkSelector "readAttributeNumberOfRFIDUsersSupportedWithParams:"

-- | @Selector@ for @readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams:@
readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParamsSelector :: Selector
readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParamsSelector = mkSelector "readAttributeNumberOfWeekDaySchedulesSupportedPerUserWithParams:"

-- | @Selector@ for @readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams:@
readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParamsSelector :: Selector
readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParamsSelector = mkSelector "readAttributeNumberOfYearDaySchedulesSupportedPerUserWithParams:"

-- | @Selector@ for @readAttributeNumberOfHolidaySchedulesSupportedWithParams:@
readAttributeNumberOfHolidaySchedulesSupportedWithParamsSelector :: Selector
readAttributeNumberOfHolidaySchedulesSupportedWithParamsSelector = mkSelector "readAttributeNumberOfHolidaySchedulesSupportedWithParams:"

-- | @Selector@ for @readAttributeMaxPINCodeLengthWithParams:@
readAttributeMaxPINCodeLengthWithParamsSelector :: Selector
readAttributeMaxPINCodeLengthWithParamsSelector = mkSelector "readAttributeMaxPINCodeLengthWithParams:"

-- | @Selector@ for @readAttributeMinPINCodeLengthWithParams:@
readAttributeMinPINCodeLengthWithParamsSelector :: Selector
readAttributeMinPINCodeLengthWithParamsSelector = mkSelector "readAttributeMinPINCodeLengthWithParams:"

-- | @Selector@ for @readAttributeMaxRFIDCodeLengthWithParams:@
readAttributeMaxRFIDCodeLengthWithParamsSelector :: Selector
readAttributeMaxRFIDCodeLengthWithParamsSelector = mkSelector "readAttributeMaxRFIDCodeLengthWithParams:"

-- | @Selector@ for @readAttributeMinRFIDCodeLengthWithParams:@
readAttributeMinRFIDCodeLengthWithParamsSelector :: Selector
readAttributeMinRFIDCodeLengthWithParamsSelector = mkSelector "readAttributeMinRFIDCodeLengthWithParams:"

-- | @Selector@ for @readAttributeCredentialRulesSupportWithParams:@
readAttributeCredentialRulesSupportWithParamsSelector :: Selector
readAttributeCredentialRulesSupportWithParamsSelector = mkSelector "readAttributeCredentialRulesSupportWithParams:"

-- | @Selector@ for @readAttributeNumberOfCredentialsSupportedPerUserWithParams:@
readAttributeNumberOfCredentialsSupportedPerUserWithParamsSelector :: Selector
readAttributeNumberOfCredentialsSupportedPerUserWithParamsSelector = mkSelector "readAttributeNumberOfCredentialsSupportedPerUserWithParams:"

-- | @Selector@ for @readAttributeLanguageWithParams:@
readAttributeLanguageWithParamsSelector :: Selector
readAttributeLanguageWithParamsSelector = mkSelector "readAttributeLanguageWithParams:"

-- | @Selector@ for @writeAttributeLanguageWithValue:expectedValueInterval:@
writeAttributeLanguageWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLanguageWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLanguageWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLanguageWithValue:expectedValueInterval:params:@
writeAttributeLanguageWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLanguageWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLanguageWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLEDSettingsWithParams:@
readAttributeLEDSettingsWithParamsSelector :: Selector
readAttributeLEDSettingsWithParamsSelector = mkSelector "readAttributeLEDSettingsWithParams:"

-- | @Selector@ for @writeAttributeLEDSettingsWithValue:expectedValueInterval:@
writeAttributeLEDSettingsWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLEDSettingsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLEDSettingsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLEDSettingsWithValue:expectedValueInterval:params:@
writeAttributeLEDSettingsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLEDSettingsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLEDSettingsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAutoRelockTimeWithParams:@
readAttributeAutoRelockTimeWithParamsSelector :: Selector
readAttributeAutoRelockTimeWithParamsSelector = mkSelector "readAttributeAutoRelockTimeWithParams:"

-- | @Selector@ for @writeAttributeAutoRelockTimeWithValue:expectedValueInterval:@
writeAttributeAutoRelockTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAutoRelockTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAutoRelockTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAutoRelockTimeWithValue:expectedValueInterval:params:@
writeAttributeAutoRelockTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAutoRelockTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAutoRelockTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSoundVolumeWithParams:@
readAttributeSoundVolumeWithParamsSelector :: Selector
readAttributeSoundVolumeWithParamsSelector = mkSelector "readAttributeSoundVolumeWithParams:"

-- | @Selector@ for @writeAttributeSoundVolumeWithValue:expectedValueInterval:@
writeAttributeSoundVolumeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSoundVolumeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSoundVolumeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSoundVolumeWithValue:expectedValueInterval:params:@
writeAttributeSoundVolumeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSoundVolumeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSoundVolumeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParamsSelector :: Selector
readAttributeOperatingModeWithParamsSelector = mkSelector "readAttributeOperatingModeWithParams:"

-- | @Selector@ for @writeAttributeOperatingModeWithValue:expectedValueInterval:@
writeAttributeOperatingModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOperatingModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOperatingModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOperatingModeWithValue:expectedValueInterval:params:@
writeAttributeOperatingModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOperatingModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOperatingModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedOperatingModesWithParams:@
readAttributeSupportedOperatingModesWithParamsSelector :: Selector
readAttributeSupportedOperatingModesWithParamsSelector = mkSelector "readAttributeSupportedOperatingModesWithParams:"

-- | @Selector@ for @readAttributeDefaultConfigurationRegisterWithParams:@
readAttributeDefaultConfigurationRegisterWithParamsSelector :: Selector
readAttributeDefaultConfigurationRegisterWithParamsSelector = mkSelector "readAttributeDefaultConfigurationRegisterWithParams:"

-- | @Selector@ for @readAttributeEnableLocalProgrammingWithParams:@
readAttributeEnableLocalProgrammingWithParamsSelector :: Selector
readAttributeEnableLocalProgrammingWithParamsSelector = mkSelector "readAttributeEnableLocalProgrammingWithParams:"

-- | @Selector@ for @writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:@
writeAttributeEnableLocalProgrammingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEnableLocalProgrammingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:params:@
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEnableLocalProgrammingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnableLocalProgrammingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnableOneTouchLockingWithParams:@
readAttributeEnableOneTouchLockingWithParamsSelector :: Selector
readAttributeEnableOneTouchLockingWithParamsSelector = mkSelector "readAttributeEnableOneTouchLockingWithParams:"

-- | @Selector@ for @writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:@
writeAttributeEnableOneTouchLockingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEnableOneTouchLockingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:params:@
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEnableOneTouchLockingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnableOneTouchLockingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnableInsideStatusLEDWithParams:@
readAttributeEnableInsideStatusLEDWithParamsSelector :: Selector
readAttributeEnableInsideStatusLEDWithParamsSelector = mkSelector "readAttributeEnableInsideStatusLEDWithParams:"

-- | @Selector@ for @writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:@
writeAttributeEnableInsideStatusLEDWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEnableInsideStatusLEDWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:params:@
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEnableInsideStatusLEDWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnableInsideStatusLEDWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnablePrivacyModeButtonWithParams:@
readAttributeEnablePrivacyModeButtonWithParamsSelector :: Selector
readAttributeEnablePrivacyModeButtonWithParamsSelector = mkSelector "readAttributeEnablePrivacyModeButtonWithParams:"

-- | @Selector@ for @writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:@
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:params:@
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEnablePrivacyModeButtonWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnablePrivacyModeButtonWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLocalProgrammingFeaturesWithParams:@
readAttributeLocalProgrammingFeaturesWithParamsSelector :: Selector
readAttributeLocalProgrammingFeaturesWithParamsSelector = mkSelector "readAttributeLocalProgrammingFeaturesWithParams:"

-- | @Selector@ for @writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:@
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:params:@
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalProgrammingFeaturesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalProgrammingFeaturesWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeWrongCodeEntryLimitWithParams:@
readAttributeWrongCodeEntryLimitWithParamsSelector :: Selector
readAttributeWrongCodeEntryLimitWithParamsSelector = mkSelector "readAttributeWrongCodeEntryLimitWithParams:"

-- | @Selector@ for @writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:@
writeAttributeWrongCodeEntryLimitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeWrongCodeEntryLimitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:params:@
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeWrongCodeEntryLimitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeWrongCodeEntryLimitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUserCodeTemporaryDisableTimeWithParams:@
readAttributeUserCodeTemporaryDisableTimeWithParamsSelector :: Selector
readAttributeUserCodeTemporaryDisableTimeWithParamsSelector = mkSelector "readAttributeUserCodeTemporaryDisableTimeWithParams:"

-- | @Selector@ for @writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:@
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:params:@
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUserCodeTemporaryDisableTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUserCodeTemporaryDisableTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSendPINOverTheAirWithParams:@
readAttributeSendPINOverTheAirWithParamsSelector :: Selector
readAttributeSendPINOverTheAirWithParamsSelector = mkSelector "readAttributeSendPINOverTheAirWithParams:"

-- | @Selector@ for @writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:@
writeAttributeSendPINOverTheAirWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSendPINOverTheAirWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:params:@
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSendPINOverTheAirWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSendPINOverTheAirWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRequirePINforRemoteOperationWithParams:@
readAttributeRequirePINforRemoteOperationWithParamsSelector :: Selector
readAttributeRequirePINforRemoteOperationWithParamsSelector = mkSelector "readAttributeRequirePINforRemoteOperationWithParams:"

-- | @Selector@ for @writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:@
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:params:@
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRequirePINforRemoteOperationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRequirePINforRemoteOperationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeExpiringUserTimeoutWithParams:@
readAttributeExpiringUserTimeoutWithParamsSelector :: Selector
readAttributeExpiringUserTimeoutWithParamsSelector = mkSelector "readAttributeExpiringUserTimeoutWithParams:"

-- | @Selector@ for @writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:@
writeAttributeExpiringUserTimeoutWithValue_expectedValueIntervalSelector :: Selector
writeAttributeExpiringUserTimeoutWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:params:@
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeExpiringUserTimeoutWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeExpiringUserTimeoutWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAliroReaderVerificationKeyWithParams:@
readAttributeAliroReaderVerificationKeyWithParamsSelector :: Selector
readAttributeAliroReaderVerificationKeyWithParamsSelector = mkSelector "readAttributeAliroReaderVerificationKeyWithParams:"

-- | @Selector@ for @readAttributeAliroReaderGroupIdentifierWithParams:@
readAttributeAliroReaderGroupIdentifierWithParamsSelector :: Selector
readAttributeAliroReaderGroupIdentifierWithParamsSelector = mkSelector "readAttributeAliroReaderGroupIdentifierWithParams:"

-- | @Selector@ for @readAttributeAliroReaderGroupSubIdentifierWithParams:@
readAttributeAliroReaderGroupSubIdentifierWithParamsSelector :: Selector
readAttributeAliroReaderGroupSubIdentifierWithParamsSelector = mkSelector "readAttributeAliroReaderGroupSubIdentifierWithParams:"

-- | @Selector@ for @readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams:@
readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector :: Selector
readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector = mkSelector "readAttributeAliroExpeditedTransactionSupportedProtocolVersionsWithParams:"

-- | @Selector@ for @readAttributeAliroGroupResolvingKeyWithParams:@
readAttributeAliroGroupResolvingKeyWithParamsSelector :: Selector
readAttributeAliroGroupResolvingKeyWithParamsSelector = mkSelector "readAttributeAliroGroupResolvingKeyWithParams:"

-- | @Selector@ for @readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams:@
readAttributeAliroSupportedBLEUWBProtocolVersionsWithParamsSelector :: Selector
readAttributeAliroSupportedBLEUWBProtocolVersionsWithParamsSelector = mkSelector "readAttributeAliroSupportedBLEUWBProtocolVersionsWithParams:"

-- | @Selector@ for @readAttributeAliroBLEAdvertisingVersionWithParams:@
readAttributeAliroBLEAdvertisingVersionWithParamsSelector :: Selector
readAttributeAliroBLEAdvertisingVersionWithParamsSelector = mkSelector "readAttributeAliroBLEAdvertisingVersionWithParams:"

-- | @Selector@ for @readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams:@
readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector :: Selector
readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector = mkSelector "readAttributeNumberOfAliroCredentialIssuerKeysSupportedWithParams:"

-- | @Selector@ for @readAttributeNumberOfAliroEndpointKeysSupportedWithParams:@
readAttributeNumberOfAliroEndpointKeysSupportedWithParamsSelector :: Selector
readAttributeNumberOfAliroEndpointKeysSupportedWithParamsSelector = mkSelector "readAttributeNumberOfAliroEndpointKeysSupportedWithParams:"

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

-- | @Selector@ for @readAttributeAppleAliroReaderVerificationKeyWithParams:@
readAttributeAppleAliroReaderVerificationKeyWithParamsSelector :: Selector
readAttributeAppleAliroReaderVerificationKeyWithParamsSelector = mkSelector "readAttributeAppleAliroReaderVerificationKeyWithParams:"

-- | @Selector@ for @readAttributeAppleAliroReaderGroupIdentifierWithParams:@
readAttributeAppleAliroReaderGroupIdentifierWithParamsSelector :: Selector
readAttributeAppleAliroReaderGroupIdentifierWithParamsSelector = mkSelector "readAttributeAppleAliroReaderGroupIdentifierWithParams:"

-- | @Selector@ for @readAttributeAppleAliroReaderGroupSubIdentifierWithParams:@
readAttributeAppleAliroReaderGroupSubIdentifierWithParamsSelector :: Selector
readAttributeAppleAliroReaderGroupSubIdentifierWithParamsSelector = mkSelector "readAttributeAppleAliroReaderGroupSubIdentifierWithParams:"

-- | @Selector@ for @readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams:@
readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector :: Selector
readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParamsSelector = mkSelector "readAttributeAppleAliroExpeditedTransactionSupportedProtocolVersionsWithParams:"

-- | @Selector@ for @readAttributeAppleAliroGroupResolvingKeyWithParams:@
readAttributeAppleAliroGroupResolvingKeyWithParamsSelector :: Selector
readAttributeAppleAliroGroupResolvingKeyWithParamsSelector = mkSelector "readAttributeAppleAliroGroupResolvingKeyWithParams:"

-- | @Selector@ for @readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams:@
readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParamsSelector :: Selector
readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParamsSelector = mkSelector "readAttributeAppleAliroSupportedBLEUWBProtocolVersionsWithParams:"

-- | @Selector@ for @readAttributeAppleAliroBLEAdvertisingVersionWithParams:@
readAttributeAppleAliroBLEAdvertisingVersionWithParamsSelector :: Selector
readAttributeAppleAliroBLEAdvertisingVersionWithParamsSelector = mkSelector "readAttributeAppleAliroBLEAdvertisingVersionWithParams:"

-- | @Selector@ for @readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams:@
readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector :: Selector
readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParamsSelector = mkSelector "readAttributeAppleNumberOfAliroCredentialIssuerKeysSupportedWithParams:"

-- | @Selector@ for @readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams:@
readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParamsSelector :: Selector
readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParamsSelector = mkSelector "readAttributeAppleNumberOfAliroEndpointKeysSupportedWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @lockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:@
lockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
lockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "lockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @unlockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:@
unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
unlockDoorWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "unlockDoorWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
unlockWithTimeoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "unlockWithTimeoutWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearWeekDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearWeekDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearYearDayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearYearDayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearHolidayScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearHolidayScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
setUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setUserWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
getUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getUserWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearUserWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearUserWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearUserWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:@
setCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getCredentialStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getCredentialStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getCredentialStatusWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearCredentialWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearCredentialWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

