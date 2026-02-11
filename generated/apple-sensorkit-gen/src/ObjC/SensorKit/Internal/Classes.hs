{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SensorKit.Internal.Classes (
    module ObjC.SensorKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SRAcousticSettings ----------

-- | Phantom type for @SRAcousticSettings@.
data SRAcousticSettings

instance IsObjCObject (Id SRAcousticSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAcousticSettings"

class IsNSObject a => IsSRAcousticSettings a where
  toSRAcousticSettings :: a -> Id SRAcousticSettings

instance IsSRAcousticSettings (Id SRAcousticSettings) where
  toSRAcousticSettings = unsafeCastId

instance IsNSObject (Id SRAcousticSettings) where
  toNSObject = unsafeCastId

-- ---------- SRAcousticSettingsAccessibility ----------

-- | Phantom type for @SRAcousticSettingsAccessibility@.
data SRAcousticSettingsAccessibility

instance IsObjCObject (Id SRAcousticSettingsAccessibility) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAcousticSettingsAccessibility"

class IsNSObject a => IsSRAcousticSettingsAccessibility a where
  toSRAcousticSettingsAccessibility :: a -> Id SRAcousticSettingsAccessibility

instance IsSRAcousticSettingsAccessibility (Id SRAcousticSettingsAccessibility) where
  toSRAcousticSettingsAccessibility = unsafeCastId

instance IsNSObject (Id SRAcousticSettingsAccessibility) where
  toNSObject = unsafeCastId

-- ---------- SRAcousticSettingsAccessibilityBackgroundSounds ----------

-- | Phantom type for @SRAcousticSettingsAccessibilityBackgroundSounds@.
data SRAcousticSettingsAccessibilityBackgroundSounds

instance IsObjCObject (Id SRAcousticSettingsAccessibilityBackgroundSounds) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAcousticSettingsAccessibilityBackgroundSounds"

class IsNSObject a => IsSRAcousticSettingsAccessibilityBackgroundSounds a where
  toSRAcousticSettingsAccessibilityBackgroundSounds :: a -> Id SRAcousticSettingsAccessibilityBackgroundSounds

instance IsSRAcousticSettingsAccessibilityBackgroundSounds (Id SRAcousticSettingsAccessibilityBackgroundSounds) where
  toSRAcousticSettingsAccessibilityBackgroundSounds = unsafeCastId

instance IsNSObject (Id SRAcousticSettingsAccessibilityBackgroundSounds) where
  toNSObject = unsafeCastId

-- ---------- SRAcousticSettingsAccessibilityHeadphoneAccommodations ----------

-- | Phantom type for @SRAcousticSettingsAccessibilityHeadphoneAccommodations@.
data SRAcousticSettingsAccessibilityHeadphoneAccommodations

instance IsObjCObject (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAcousticSettingsAccessibilityHeadphoneAccommodations"

class IsNSObject a => IsSRAcousticSettingsAccessibilityHeadphoneAccommodations a where
  toSRAcousticSettingsAccessibilityHeadphoneAccommodations :: a -> Id SRAcousticSettingsAccessibilityHeadphoneAccommodations

instance IsSRAcousticSettingsAccessibilityHeadphoneAccommodations (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations) where
  toSRAcousticSettingsAccessibilityHeadphoneAccommodations = unsafeCastId

instance IsNSObject (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations) where
  toNSObject = unsafeCastId

-- ---------- SRAcousticSettingsMusicEQ ----------

-- | Phantom type for @SRAcousticSettingsMusicEQ@.
data SRAcousticSettingsMusicEQ

instance IsObjCObject (Id SRAcousticSettingsMusicEQ) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAcousticSettingsMusicEQ"

class IsNSObject a => IsSRAcousticSettingsMusicEQ a where
  toSRAcousticSettingsMusicEQ :: a -> Id SRAcousticSettingsMusicEQ

instance IsSRAcousticSettingsMusicEQ (Id SRAcousticSettingsMusicEQ) where
  toSRAcousticSettingsMusicEQ = unsafeCastId

instance IsNSObject (Id SRAcousticSettingsMusicEQ) where
  toNSObject = unsafeCastId

-- ---------- SRAmbientLightSample ----------

-- | Phantom type for @SRAmbientLightSample@.
data SRAmbientLightSample

instance IsObjCObject (Id SRAmbientLightSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAmbientLightSample"

class IsNSObject a => IsSRAmbientLightSample a where
  toSRAmbientLightSample :: a -> Id SRAmbientLightSample

instance IsSRAmbientLightSample (Id SRAmbientLightSample) where
  toSRAmbientLightSample = unsafeCastId

instance IsNSObject (Id SRAmbientLightSample) where
  toNSObject = unsafeCastId

-- ---------- SRApplicationUsage ----------

-- | Phantom type for @SRApplicationUsage@.
data SRApplicationUsage

instance IsObjCObject (Id SRApplicationUsage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRApplicationUsage"

class IsNSObject a => IsSRApplicationUsage a where
  toSRApplicationUsage :: a -> Id SRApplicationUsage

instance IsSRApplicationUsage (Id SRApplicationUsage) where
  toSRApplicationUsage = unsafeCastId

instance IsNSObject (Id SRApplicationUsage) where
  toNSObject = unsafeCastId

-- ---------- SRAudioLevel ----------

-- | Phantom type for @SRAudioLevel@.
data SRAudioLevel

instance IsObjCObject (Id SRAudioLevel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRAudioLevel"

class IsNSObject a => IsSRAudioLevel a where
  toSRAudioLevel :: a -> Id SRAudioLevel

instance IsSRAudioLevel (Id SRAudioLevel) where
  toSRAudioLevel = unsafeCastId

instance IsNSObject (Id SRAudioLevel) where
  toNSObject = unsafeCastId

-- ---------- SRDeletionRecord ----------

-- | Phantom type for @SRDeletionRecord@.
data SRDeletionRecord

instance IsObjCObject (Id SRDeletionRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRDeletionRecord"

class IsNSObject a => IsSRDeletionRecord a where
  toSRDeletionRecord :: a -> Id SRDeletionRecord

instance IsSRDeletionRecord (Id SRDeletionRecord) where
  toSRDeletionRecord = unsafeCastId

instance IsNSObject (Id SRDeletionRecord) where
  toNSObject = unsafeCastId

-- ---------- SRDevice ----------

-- | Phantom type for @SRDevice@.
data SRDevice

instance IsObjCObject (Id SRDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRDevice"

class IsNSObject a => IsSRDevice a where
  toSRDevice :: a -> Id SRDevice

instance IsSRDevice (Id SRDevice) where
  toSRDevice = unsafeCastId

instance IsNSObject (Id SRDevice) where
  toNSObject = unsafeCastId

-- ---------- SRDeviceUsageReport ----------

-- | Phantom type for @SRDeviceUsageReport@.
data SRDeviceUsageReport

instance IsObjCObject (Id SRDeviceUsageReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRDeviceUsageReport"

class IsNSObject a => IsSRDeviceUsageReport a where
  toSRDeviceUsageReport :: a -> Id SRDeviceUsageReport

instance IsSRDeviceUsageReport (Id SRDeviceUsageReport) where
  toSRDeviceUsageReport = unsafeCastId

instance IsNSObject (Id SRDeviceUsageReport) where
  toNSObject = unsafeCastId

-- ---------- SRElectrocardiogramData ----------

-- | Phantom type for @SRElectrocardiogramData@.
data SRElectrocardiogramData

instance IsObjCObject (Id SRElectrocardiogramData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRElectrocardiogramData"

class IsNSObject a => IsSRElectrocardiogramData a where
  toSRElectrocardiogramData :: a -> Id SRElectrocardiogramData

instance IsSRElectrocardiogramData (Id SRElectrocardiogramData) where
  toSRElectrocardiogramData = unsafeCastId

instance IsNSObject (Id SRElectrocardiogramData) where
  toNSObject = unsafeCastId

-- ---------- SRElectrocardiogramSample ----------

-- | Phantom type for @SRElectrocardiogramSample@.
data SRElectrocardiogramSample

instance IsObjCObject (Id SRElectrocardiogramSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRElectrocardiogramSample"

class IsNSObject a => IsSRElectrocardiogramSample a where
  toSRElectrocardiogramSample :: a -> Id SRElectrocardiogramSample

instance IsSRElectrocardiogramSample (Id SRElectrocardiogramSample) where
  toSRElectrocardiogramSample = unsafeCastId

instance IsNSObject (Id SRElectrocardiogramSample) where
  toNSObject = unsafeCastId

-- ---------- SRElectrocardiogramSession ----------

-- | Phantom type for @SRElectrocardiogramSession@.
data SRElectrocardiogramSession

instance IsObjCObject (Id SRElectrocardiogramSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRElectrocardiogramSession"

class IsNSObject a => IsSRElectrocardiogramSession a where
  toSRElectrocardiogramSession :: a -> Id SRElectrocardiogramSession

instance IsSRElectrocardiogramSession (Id SRElectrocardiogramSession) where
  toSRElectrocardiogramSession = unsafeCastId

instance IsNSObject (Id SRElectrocardiogramSession) where
  toNSObject = unsafeCastId

-- ---------- SRFaceMetrics ----------

-- | Phantom type for @SRFaceMetrics@.
data SRFaceMetrics

instance IsObjCObject (Id SRFaceMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRFaceMetrics"

class IsNSObject a => IsSRFaceMetrics a where
  toSRFaceMetrics :: a -> Id SRFaceMetrics

instance IsSRFaceMetrics (Id SRFaceMetrics) where
  toSRFaceMetrics = unsafeCastId

instance IsNSObject (Id SRFaceMetrics) where
  toNSObject = unsafeCastId

-- ---------- SRFaceMetricsExpression ----------

-- | Phantom type for @SRFaceMetricsExpression@.
data SRFaceMetricsExpression

instance IsObjCObject (Id SRFaceMetricsExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRFaceMetricsExpression"

class IsNSObject a => IsSRFaceMetricsExpression a where
  toSRFaceMetricsExpression :: a -> Id SRFaceMetricsExpression

instance IsSRFaceMetricsExpression (Id SRFaceMetricsExpression) where
  toSRFaceMetricsExpression = unsafeCastId

instance IsNSObject (Id SRFaceMetricsExpression) where
  toNSObject = unsafeCastId

-- ---------- SRFetchRequest ----------

-- | Phantom type for @SRFetchRequest@.
data SRFetchRequest

instance IsObjCObject (Id SRFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRFetchRequest"

class IsNSObject a => IsSRFetchRequest a where
  toSRFetchRequest :: a -> Id SRFetchRequest

instance IsSRFetchRequest (Id SRFetchRequest) where
  toSRFetchRequest = unsafeCastId

instance IsNSObject (Id SRFetchRequest) where
  toNSObject = unsafeCastId

-- ---------- SRFetchResult ----------

-- | Phantom type for @SRFetchResult@.
data SRFetchResult

instance IsObjCObject (Id SRFetchResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRFetchResult"

class IsNSObject a => IsSRFetchResult a where
  toSRFetchResult :: a -> Id SRFetchResult

instance IsSRFetchResult (Id SRFetchResult) where
  toSRFetchResult = unsafeCastId

instance IsNSObject (Id SRFetchResult) where
  toNSObject = unsafeCastId

-- ---------- SRKeyboardMetrics ----------

-- | Phantom type for @SRKeyboardMetrics@.
data SRKeyboardMetrics

instance IsObjCObject (Id SRKeyboardMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRKeyboardMetrics"

class IsNSObject a => IsSRKeyboardMetrics a where
  toSRKeyboardMetrics :: a -> Id SRKeyboardMetrics

instance IsSRKeyboardMetrics (Id SRKeyboardMetrics) where
  toSRKeyboardMetrics = unsafeCastId

instance IsNSObject (Id SRKeyboardMetrics) where
  toNSObject = unsafeCastId

-- ---------- SRKeyboardProbabilityMetric ----------

-- | Phantom type for @SRKeyboardProbabilityMetric@.
data SRKeyboardProbabilityMetric

instance IsObjCObject (Id SRKeyboardProbabilityMetric) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRKeyboardProbabilityMetric"

class IsNSObject a => IsSRKeyboardProbabilityMetric a where
  toSRKeyboardProbabilityMetric :: a -> Id SRKeyboardProbabilityMetric

instance IsSRKeyboardProbabilityMetric (Id SRKeyboardProbabilityMetric) where
  toSRKeyboardProbabilityMetric = unsafeCastId

instance IsNSObject (Id SRKeyboardProbabilityMetric) where
  toNSObject = unsafeCastId

-- ---------- SRMediaEvent ----------

-- | Phantom type for @SRMediaEvent@.
data SRMediaEvent

instance IsObjCObject (Id SRMediaEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRMediaEvent"

class IsNSObject a => IsSRMediaEvent a where
  toSRMediaEvent :: a -> Id SRMediaEvent

instance IsSRMediaEvent (Id SRMediaEvent) where
  toSRMediaEvent = unsafeCastId

instance IsNSObject (Id SRMediaEvent) where
  toNSObject = unsafeCastId

-- ---------- SRMessagesUsageReport ----------

-- | Phantom type for @SRMessagesUsageReport@.
data SRMessagesUsageReport

instance IsObjCObject (Id SRMessagesUsageReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRMessagesUsageReport"

class IsNSObject a => IsSRMessagesUsageReport a where
  toSRMessagesUsageReport :: a -> Id SRMessagesUsageReport

instance IsSRMessagesUsageReport (Id SRMessagesUsageReport) where
  toSRMessagesUsageReport = unsafeCastId

instance IsNSObject (Id SRMessagesUsageReport) where
  toNSObject = unsafeCastId

-- ---------- SRNotificationUsage ----------

-- | Phantom type for @SRNotificationUsage@.
data SRNotificationUsage

instance IsObjCObject (Id SRNotificationUsage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRNotificationUsage"

class IsNSObject a => IsSRNotificationUsage a where
  toSRNotificationUsage :: a -> Id SRNotificationUsage

instance IsSRNotificationUsage (Id SRNotificationUsage) where
  toSRNotificationUsage = unsafeCastId

instance IsNSObject (Id SRNotificationUsage) where
  toNSObject = unsafeCastId

-- ---------- SRPhoneUsageReport ----------

-- | Phantom type for @SRPhoneUsageReport@.
data SRPhoneUsageReport

instance IsObjCObject (Id SRPhoneUsageReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRPhoneUsageReport"

class IsNSObject a => IsSRPhoneUsageReport a where
  toSRPhoneUsageReport :: a -> Id SRPhoneUsageReport

instance IsSRPhoneUsageReport (Id SRPhoneUsageReport) where
  toSRPhoneUsageReport = unsafeCastId

instance IsNSObject (Id SRPhoneUsageReport) where
  toNSObject = unsafeCastId

-- ---------- SRPhotoplethysmogramAccelerometerSample ----------

-- | Phantom type for @SRPhotoplethysmogramAccelerometerSample@.
data SRPhotoplethysmogramAccelerometerSample

instance IsObjCObject (Id SRPhotoplethysmogramAccelerometerSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRPhotoplethysmogramAccelerometerSample"

class IsNSObject a => IsSRPhotoplethysmogramAccelerometerSample a where
  toSRPhotoplethysmogramAccelerometerSample :: a -> Id SRPhotoplethysmogramAccelerometerSample

instance IsSRPhotoplethysmogramAccelerometerSample (Id SRPhotoplethysmogramAccelerometerSample) where
  toSRPhotoplethysmogramAccelerometerSample = unsafeCastId

instance IsNSObject (Id SRPhotoplethysmogramAccelerometerSample) where
  toNSObject = unsafeCastId

-- ---------- SRPhotoplethysmogramOpticalSample ----------

-- | Phantom type for @SRPhotoplethysmogramOpticalSample@.
data SRPhotoplethysmogramOpticalSample

instance IsObjCObject (Id SRPhotoplethysmogramOpticalSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRPhotoplethysmogramOpticalSample"

class IsNSObject a => IsSRPhotoplethysmogramOpticalSample a where
  toSRPhotoplethysmogramOpticalSample :: a -> Id SRPhotoplethysmogramOpticalSample

instance IsSRPhotoplethysmogramOpticalSample (Id SRPhotoplethysmogramOpticalSample) where
  toSRPhotoplethysmogramOpticalSample = unsafeCastId

instance IsNSObject (Id SRPhotoplethysmogramOpticalSample) where
  toNSObject = unsafeCastId

-- ---------- SRPhotoplethysmogramSample ----------

-- | Phantom type for @SRPhotoplethysmogramSample@.
data SRPhotoplethysmogramSample

instance IsObjCObject (Id SRPhotoplethysmogramSample) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRPhotoplethysmogramSample"

class IsNSObject a => IsSRPhotoplethysmogramSample a where
  toSRPhotoplethysmogramSample :: a -> Id SRPhotoplethysmogramSample

instance IsSRPhotoplethysmogramSample (Id SRPhotoplethysmogramSample) where
  toSRPhotoplethysmogramSample = unsafeCastId

instance IsNSObject (Id SRPhotoplethysmogramSample) where
  toNSObject = unsafeCastId

-- ---------- SRSensorReader ----------

-- | Phantom type for @SRSensorReader@.
data SRSensorReader

instance IsObjCObject (Id SRSensorReader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRSensorReader"

class IsNSObject a => IsSRSensorReader a where
  toSRSensorReader :: a -> Id SRSensorReader

instance IsSRSensorReader (Id SRSensorReader) where
  toSRSensorReader = unsafeCastId

instance IsNSObject (Id SRSensorReader) where
  toNSObject = unsafeCastId

-- ---------- SRSleepSession ----------

-- | Phantom type for @SRSleepSession@.
data SRSleepSession

instance IsObjCObject (Id SRSleepSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRSleepSession"

class IsNSObject a => IsSRSleepSession a where
  toSRSleepSession :: a -> Id SRSleepSession

instance IsSRSleepSession (Id SRSleepSession) where
  toSRSleepSession = unsafeCastId

instance IsNSObject (Id SRSleepSession) where
  toNSObject = unsafeCastId

-- ---------- SRSpeechExpression ----------

-- | Phantom type for @SRSpeechExpression@.
data SRSpeechExpression

instance IsObjCObject (Id SRSpeechExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRSpeechExpression"

class IsNSObject a => IsSRSpeechExpression a where
  toSRSpeechExpression :: a -> Id SRSpeechExpression

instance IsSRSpeechExpression (Id SRSpeechExpression) where
  toSRSpeechExpression = unsafeCastId

instance IsNSObject (Id SRSpeechExpression) where
  toNSObject = unsafeCastId

-- ---------- SRSpeechMetrics ----------

-- | Phantom type for @SRSpeechMetrics@.
data SRSpeechMetrics

instance IsObjCObject (Id SRSpeechMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRSpeechMetrics"

class IsNSObject a => IsSRSpeechMetrics a where
  toSRSpeechMetrics :: a -> Id SRSpeechMetrics

instance IsSRSpeechMetrics (Id SRSpeechMetrics) where
  toSRSpeechMetrics = unsafeCastId

instance IsNSObject (Id SRSpeechMetrics) where
  toNSObject = unsafeCastId

-- ---------- SRSupplementalCategory ----------

-- | SRSupplementalCategory
--
-- A supplemental category to provide more context than just the app category
--
-- The app categories from @SRDeviceUsageCategoryKey@ are very general. Providing a supplemental category allows more context about the specific app while not revealing the exact app identity.
-- 
-- Phantom type for @SRSupplementalCategory@.
data SRSupplementalCategory

instance IsObjCObject (Id SRSupplementalCategory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRSupplementalCategory"

class IsNSObject a => IsSRSupplementalCategory a where
  toSRSupplementalCategory :: a -> Id SRSupplementalCategory

instance IsSRSupplementalCategory (Id SRSupplementalCategory) where
  toSRSupplementalCategory = unsafeCastId

instance IsNSObject (Id SRSupplementalCategory) where
  toNSObject = unsafeCastId

-- ---------- SRTextInputSession ----------

-- | Phantom type for @SRTextInputSession@.
data SRTextInputSession

instance IsObjCObject (Id SRTextInputSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRTextInputSession"

class IsNSObject a => IsSRTextInputSession a where
  toSRTextInputSession :: a -> Id SRTextInputSession

instance IsSRTextInputSession (Id SRTextInputSession) where
  toSRTextInputSession = unsafeCastId

instance IsNSObject (Id SRTextInputSession) where
  toNSObject = unsafeCastId

-- ---------- SRVisit ----------

-- | Phantom type for @SRVisit@.
data SRVisit

instance IsObjCObject (Id SRVisit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRVisit"

class IsNSObject a => IsSRVisit a where
  toSRVisit :: a -> Id SRVisit

instance IsSRVisit (Id SRVisit) where
  toSRVisit = unsafeCastId

instance IsNSObject (Id SRVisit) where
  toNSObject = unsafeCastId

-- ---------- SRWebUsage ----------

-- | Phantom type for @SRWebUsage@.
data SRWebUsage

instance IsObjCObject (Id SRWebUsage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRWebUsage"

class IsNSObject a => IsSRWebUsage a where
  toSRWebUsage :: a -> Id SRWebUsage

instance IsSRWebUsage (Id SRWebUsage) where
  toSRWebUsage = unsafeCastId

instance IsNSObject (Id SRWebUsage) where
  toNSObject = unsafeCastId

-- ---------- SRWristDetection ----------

-- | Phantom type for @SRWristDetection@.
data SRWristDetection

instance IsObjCObject (Id SRWristDetection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRWristDetection"

class IsNSObject a => IsSRWristDetection a where
  toSRWristDetection :: a -> Id SRWristDetection

instance IsSRWristDetection (Id SRWristDetection) where
  toSRWristDetection = unsafeCastId

instance IsNSObject (Id SRWristDetection) where
  toNSObject = unsafeCastId

-- ---------- SRWristTemperature ----------

-- | Phantom type for @SRWristTemperature@.
data SRWristTemperature

instance IsObjCObject (Id SRWristTemperature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRWristTemperature"

class IsNSObject a => IsSRWristTemperature a where
  toSRWristTemperature :: a -> Id SRWristTemperature

instance IsSRWristTemperature (Id SRWristTemperature) where
  toSRWristTemperature = unsafeCastId

instance IsNSObject (Id SRWristTemperature) where
  toNSObject = unsafeCastId

-- ---------- SRWristTemperatureSession ----------

-- | Phantom type for @SRWristTemperatureSession@.
data SRWristTemperatureSession

instance IsObjCObject (Id SRWristTemperatureSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SRWristTemperatureSession"

class IsNSObject a => IsSRWristTemperatureSession a where
  toSRWristTemperatureSession :: a -> Id SRWristTemperatureSession

instance IsSRWristTemperatureSession (Id SRWristTemperatureSession) where
  toSRWristTemperatureSession = unsafeCastId

instance IsNSObject (Id SRWristTemperatureSession) where
  toNSObject = unsafeCastId
