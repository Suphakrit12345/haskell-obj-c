{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKObjectType
--
-- An abstract class representing a type of object that can be stored by HealthKit.
--
-- Generated bindings for @HKObjectType@.
module ObjC.HealthKit.HKObjectType
  ( HKObjectType
  , IsHKObjectType(..)
  , init_
  , quantityTypeForIdentifier
  , categoryTypeForIdentifier
  , characteristicTypeForIdentifier
  , correlationTypeForIdentifier
  , documentTypeForIdentifier
  , scoredAssessmentTypeForIdentifier
  , seriesTypeForIdentifier
  , workoutType
  , activitySummaryType
  , audiogramSampleType
  , electrocardiogramType
  , medicationDoseEventType
  , visionPrescriptionType
  , stateOfMindType
  , userAnnotatedMedicationType
  , requiresPerObjectAuthorization
  , clinicalTypeForIdentifier
  , identifier
  , initSelector
  , quantityTypeForIdentifierSelector
  , categoryTypeForIdentifierSelector
  , characteristicTypeForIdentifierSelector
  , correlationTypeForIdentifierSelector
  , documentTypeForIdentifierSelector
  , scoredAssessmentTypeForIdentifierSelector
  , seriesTypeForIdentifierSelector
  , workoutTypeSelector
  , activitySummaryTypeSelector
  , audiogramSampleTypeSelector
  , electrocardiogramTypeSelector
  , medicationDoseEventTypeSelector
  , visionPrescriptionTypeSelector
  , stateOfMindTypeSelector
  , userAnnotatedMedicationTypeSelector
  , requiresPerObjectAuthorizationSelector
  , clinicalTypeForIdentifierSelector
  , identifierSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKObjectType hkObjectType => hkObjectType -> IO (Id HKObjectType)
init_ hkObjectType  =
    sendMsg hkObjectType (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ quantityTypeForIdentifier:@
quantityTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKQuantityType)
quantityTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "quantityTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ categoryTypeForIdentifier:@
categoryTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKCategoryType)
categoryTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "categoryTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ characteristicTypeForIdentifier:@
characteristicTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKCharacteristicType)
characteristicTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "characteristicTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ correlationTypeForIdentifier:@
correlationTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKCorrelationType)
correlationTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "correlationTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ documentTypeForIdentifier:@
documentTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKDocumentType)
documentTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "documentTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ scoredAssessmentTypeForIdentifier:@
scoredAssessmentTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKScoredAssessmentType)
scoredAssessmentTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "scoredAssessmentTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ seriesTypeForIdentifier:@
seriesTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKSeriesType)
seriesTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "seriesTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ workoutType@
workoutType :: IO (Id HKWorkoutType)
workoutType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "workoutType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ activitySummaryType@
activitySummaryType :: IO (Id HKActivitySummaryType)
activitySummaryType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "activitySummaryType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ audiogramSampleType@
audiogramSampleType :: IO (Id HKAudiogramSampleType)
audiogramSampleType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "audiogramSampleType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ electrocardiogramType@
electrocardiogramType :: IO (Id HKElectrocardiogramType)
electrocardiogramType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "electrocardiogramType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ medicationDoseEventType@
medicationDoseEventType :: IO (Id HKMedicationDoseEventType)
medicationDoseEventType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "medicationDoseEventType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ visionPrescriptionType@
visionPrescriptionType :: IO (Id HKPrescriptionType)
visionPrescriptionType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "visionPrescriptionType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ stateOfMindType@
stateOfMindType :: IO (Id HKStateOfMindType)
stateOfMindType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "stateOfMindType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ userAnnotatedMedicationType@
userAnnotatedMedicationType :: IO (Id HKUserAnnotatedMedicationType)
userAnnotatedMedicationType  =
  do
    cls' <- getRequiredClass "HKObjectType"
    sendClassMsg cls' (mkSelector "userAnnotatedMedicationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | requiresPerObjectAuthorization
--
-- Returns YES if the authorization for the object type needs to be requested on per object basis.
--
-- ObjC selector: @- requiresPerObjectAuthorization@
requiresPerObjectAuthorization :: IsHKObjectType hkObjectType => hkObjectType -> IO Bool
requiresPerObjectAuthorization hkObjectType  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkObjectType (mkSelector "requiresPerObjectAuthorization") retCULong []

-- | @+ clinicalTypeForIdentifier:@
clinicalTypeForIdentifier :: IsNSString identifier => identifier -> IO (Id HKClinicalType)
clinicalTypeForIdentifier identifier =
  do
    cls' <- getRequiredClass "HKObjectType"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "clinicalTypeForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | identifier
--
-- A unique string identifying a type of health object.
--
-- See HKTypeIdentifiers.h for possible values.
--
-- ObjC selector: @- identifier@
identifier :: IsHKObjectType hkObjectType => hkObjectType -> IO (Id NSString)
identifier hkObjectType  =
    sendMsg hkObjectType (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @quantityTypeForIdentifier:@
quantityTypeForIdentifierSelector :: Selector
quantityTypeForIdentifierSelector = mkSelector "quantityTypeForIdentifier:"

-- | @Selector@ for @categoryTypeForIdentifier:@
categoryTypeForIdentifierSelector :: Selector
categoryTypeForIdentifierSelector = mkSelector "categoryTypeForIdentifier:"

-- | @Selector@ for @characteristicTypeForIdentifier:@
characteristicTypeForIdentifierSelector :: Selector
characteristicTypeForIdentifierSelector = mkSelector "characteristicTypeForIdentifier:"

-- | @Selector@ for @correlationTypeForIdentifier:@
correlationTypeForIdentifierSelector :: Selector
correlationTypeForIdentifierSelector = mkSelector "correlationTypeForIdentifier:"

-- | @Selector@ for @documentTypeForIdentifier:@
documentTypeForIdentifierSelector :: Selector
documentTypeForIdentifierSelector = mkSelector "documentTypeForIdentifier:"

-- | @Selector@ for @scoredAssessmentTypeForIdentifier:@
scoredAssessmentTypeForIdentifierSelector :: Selector
scoredAssessmentTypeForIdentifierSelector = mkSelector "scoredAssessmentTypeForIdentifier:"

-- | @Selector@ for @seriesTypeForIdentifier:@
seriesTypeForIdentifierSelector :: Selector
seriesTypeForIdentifierSelector = mkSelector "seriesTypeForIdentifier:"

-- | @Selector@ for @workoutType@
workoutTypeSelector :: Selector
workoutTypeSelector = mkSelector "workoutType"

-- | @Selector@ for @activitySummaryType@
activitySummaryTypeSelector :: Selector
activitySummaryTypeSelector = mkSelector "activitySummaryType"

-- | @Selector@ for @audiogramSampleType@
audiogramSampleTypeSelector :: Selector
audiogramSampleTypeSelector = mkSelector "audiogramSampleType"

-- | @Selector@ for @electrocardiogramType@
electrocardiogramTypeSelector :: Selector
electrocardiogramTypeSelector = mkSelector "electrocardiogramType"

-- | @Selector@ for @medicationDoseEventType@
medicationDoseEventTypeSelector :: Selector
medicationDoseEventTypeSelector = mkSelector "medicationDoseEventType"

-- | @Selector@ for @visionPrescriptionType@
visionPrescriptionTypeSelector :: Selector
visionPrescriptionTypeSelector = mkSelector "visionPrescriptionType"

-- | @Selector@ for @stateOfMindType@
stateOfMindTypeSelector :: Selector
stateOfMindTypeSelector = mkSelector "stateOfMindType"

-- | @Selector@ for @userAnnotatedMedicationType@
userAnnotatedMedicationTypeSelector :: Selector
userAnnotatedMedicationTypeSelector = mkSelector "userAnnotatedMedicationType"

-- | @Selector@ for @requiresPerObjectAuthorization@
requiresPerObjectAuthorizationSelector :: Selector
requiresPerObjectAuthorizationSelector = mkSelector "requiresPerObjectAuthorization"

-- | @Selector@ for @clinicalTypeForIdentifier:@
clinicalTypeForIdentifierSelector :: Selector
clinicalTypeForIdentifierSelector = mkSelector "clinicalTypeForIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

