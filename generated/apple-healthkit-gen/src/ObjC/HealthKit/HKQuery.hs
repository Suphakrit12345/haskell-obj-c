{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKQuery@.
module ObjC.HealthKit.HKQuery
  ( HKQuery
  , IsHKQuery(..)
  , init_
  , predicateForUserAnnotatedMedicationsWithIsArchived
  , predicateForUserAnnotatedMedicationsWithHasSchedule
  , predicateForMedicationDoseEventWithStatus
  , predicateForMedicationDoseEventWithStatuses
  , predicateForMedicationDoseEventWithScheduledDate
  , predicateForMedicationDoseEventWithScheduledDates
  , predicateForMedicationDoseEventWithScheduledStartDate_endDate
  , predicateForMedicationDoseEventWithMedicationConceptIdentifier
  , predicateForMedicationDoseEventWithMedicationConceptIdentifiers
  , predicateForStatesOfMindWithValence_operatorType
  , predicateForStatesOfMindWithKind
  , predicateForStatesOfMindWithLabel
  , predicateForStatesOfMindWithAssociation
  , predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval
  , predicateForElectrocardiogramsWithClassification
  , predicateForElectrocardiogramsWithSymptomsStatus
  , predicateForClinicalRecordsWithFHIRResourceType
  , predicateForClinicalRecordsFromSource_FHIRResourceType_identifier
  , predicateForActivitySummaryWithDateComponents
  , predicateForActivitySummariesBetweenStartDateComponents_endDateComponents
  , predicateForWorkoutActivitiesWithWorkoutActivityType
  , predicateForWorkoutActivitiesWithOperatorType_duration
  , predicateForWorkoutActivitiesWithStartDate_endDate_options
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity
  , predicateForWorkoutsWithActivityPredicate
  , predicateForWorkoutsWithWorkoutActivityType
  , predicateForWorkoutsWithOperatorType_duration
  , predicateForWorkoutsWithOperatorType_totalEnergyBurned
  , predicateForWorkoutsWithOperatorType_totalDistance
  , predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount
  , predicateForWorkoutsWithOperatorType_totalFlightsClimbed
  , predicateForWorkoutsWithOperatorType_quantityType_sumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_averageQuantity
  , predicateForCategorySamplesWithOperatorType_value
  , predicateForCategorySamplesEqualToValues
  , predicateForQuantitySamplesWithOperatorType_quantity
  , predicateForSamplesWithStartDate_endDate_options
  , predicateForObjectsWithMetadataKey
  , predicateForObjectsWithMetadataKey_allowedValues
  , predicateForObjectsWithMetadataKey_operatorType_value
  , predicateForObjectsFromSource
  , predicateForObjectsFromSources
  , predicateForObjectsFromSourceRevisions
  , predicateForObjectsFromDevices
  , predicateForObjectsWithDeviceProperty_allowedValues
  , predicateForObjectWithUUID
  , predicateForObjectsWithUUIDs
  , predicateForObjectsWithNoCorrelation
  , predicateForObjectsFromWorkout
  , predicateForObjectsAssociatedWithElectrocardiogram
  , predicateForWorkoutEffortSamplesRelatedToWorkout_activity
  , objectType
  , sampleType
  , predicate
  , initSelector
  , predicateForUserAnnotatedMedicationsWithIsArchivedSelector
  , predicateForUserAnnotatedMedicationsWithHasScheduleSelector
  , predicateForMedicationDoseEventWithStatusSelector
  , predicateForMedicationDoseEventWithStatusesSelector
  , predicateForMedicationDoseEventWithScheduledDateSelector
  , predicateForMedicationDoseEventWithScheduledDatesSelector
  , predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector
  , predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector
  , predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector
  , predicateForStatesOfMindWithValence_operatorTypeSelector
  , predicateForStatesOfMindWithKindSelector
  , predicateForStatesOfMindWithLabelSelector
  , predicateForStatesOfMindWithAssociationSelector
  , predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector
  , predicateForElectrocardiogramsWithClassificationSelector
  , predicateForElectrocardiogramsWithSymptomsStatusSelector
  , predicateForClinicalRecordsWithFHIRResourceTypeSelector
  , predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector
  , predicateForActivitySummaryWithDateComponentsSelector
  , predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector
  , predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector
  , predicateForWorkoutActivitiesWithOperatorType_durationSelector
  , predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector
  , predicateForWorkoutsWithActivityPredicateSelector
  , predicateForWorkoutsWithWorkoutActivityTypeSelector
  , predicateForWorkoutsWithOperatorType_durationSelector
  , predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector
  , predicateForWorkoutsWithOperatorType_totalDistanceSelector
  , predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector
  , predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector
  , predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector
  , predicateForCategorySamplesWithOperatorType_valueSelector
  , predicateForCategorySamplesEqualToValuesSelector
  , predicateForQuantitySamplesWithOperatorType_quantitySelector
  , predicateForSamplesWithStartDate_endDate_optionsSelector
  , predicateForObjectsWithMetadataKeySelector
  , predicateForObjectsWithMetadataKey_allowedValuesSelector
  , predicateForObjectsWithMetadataKey_operatorType_valueSelector
  , predicateForObjectsFromSourceSelector
  , predicateForObjectsFromSourcesSelector
  , predicateForObjectsFromSourceRevisionsSelector
  , predicateForObjectsFromDevicesSelector
  , predicateForObjectsWithDeviceProperty_allowedValuesSelector
  , predicateForObjectWithUUIDSelector
  , predicateForObjectsWithUUIDsSelector
  , predicateForObjectsWithNoCorrelationSelector
  , predicateForObjectsFromWorkoutSelector
  , predicateForObjectsAssociatedWithElectrocardiogramSelector
  , predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector
  , objectTypeSelector
  , sampleTypeSelector
  , predicateSelector

  -- * Enum types
  , HKElectrocardiogramClassification(HKElectrocardiogramClassification)
  , pattern HKElectrocardiogramClassificationNotSet
  , pattern HKElectrocardiogramClassificationSinusRhythm
  , pattern HKElectrocardiogramClassificationAtrialFibrillation
  , pattern HKElectrocardiogramClassificationInconclusiveLowHeartRate
  , pattern HKElectrocardiogramClassificationInconclusiveHighHeartRate
  , pattern HKElectrocardiogramClassificationInconclusivePoorReading
  , pattern HKElectrocardiogramClassificationInconclusiveOther
  , pattern HKElectrocardiogramClassificationUnrecognized
  , HKElectrocardiogramSymptomsStatus(HKElectrocardiogramSymptomsStatus)
  , pattern HKElectrocardiogramSymptomsStatusNotSet
  , pattern HKElectrocardiogramSymptomsStatusNone
  , pattern HKElectrocardiogramSymptomsStatusPresent
  , HKMedicationDoseEventLogStatus(HKMedicationDoseEventLogStatus)
  , pattern HKMedicationDoseEventLogStatusNotInteracted
  , pattern HKMedicationDoseEventLogStatusNotificationNotSent
  , pattern HKMedicationDoseEventLogStatusSnoozed
  , pattern HKMedicationDoseEventLogStatusTaken
  , pattern HKMedicationDoseEventLogStatusSkipped
  , pattern HKMedicationDoseEventLogStatusNotLogged
  , HKQueryOptions(HKQueryOptions)
  , pattern HKQueryOptionNone
  , pattern HKQueryOptionStrictStartDate
  , pattern HKQueryOptionStrictEndDate
  , HKStateOfMindAssociation(HKStateOfMindAssociation)
  , pattern HKStateOfMindAssociationCommunity
  , pattern HKStateOfMindAssociationCurrentEvents
  , pattern HKStateOfMindAssociationDating
  , pattern HKStateOfMindAssociationEducation
  , pattern HKStateOfMindAssociationFamily
  , pattern HKStateOfMindAssociationFitness
  , pattern HKStateOfMindAssociationFriends
  , pattern HKStateOfMindAssociationHealth
  , pattern HKStateOfMindAssociationHobbies
  , pattern HKStateOfMindAssociationIdentity
  , pattern HKStateOfMindAssociationMoney
  , pattern HKStateOfMindAssociationPartner
  , pattern HKStateOfMindAssociationSelfCare
  , pattern HKStateOfMindAssociationSpirituality
  , pattern HKStateOfMindAssociationTasks
  , pattern HKStateOfMindAssociationTravel
  , pattern HKStateOfMindAssociationWork
  , pattern HKStateOfMindAssociationWeather
  , HKStateOfMindKind(HKStateOfMindKind)
  , pattern HKStateOfMindKindMomentaryEmotion
  , pattern HKStateOfMindKindDailyMood
  , HKStateOfMindLabel(HKStateOfMindLabel)
  , pattern HKStateOfMindLabelAmazed
  , pattern HKStateOfMindLabelAmused
  , pattern HKStateOfMindLabelAngry
  , pattern HKStateOfMindLabelAnxious
  , pattern HKStateOfMindLabelAshamed
  , pattern HKStateOfMindLabelBrave
  , pattern HKStateOfMindLabelCalm
  , pattern HKStateOfMindLabelContent
  , pattern HKStateOfMindLabelDisappointed
  , pattern HKStateOfMindLabelDiscouraged
  , pattern HKStateOfMindLabelDisgusted
  , pattern HKStateOfMindLabelEmbarrassed
  , pattern HKStateOfMindLabelExcited
  , pattern HKStateOfMindLabelFrustrated
  , pattern HKStateOfMindLabelGrateful
  , pattern HKStateOfMindLabelGuilty
  , pattern HKStateOfMindLabelHappy
  , pattern HKStateOfMindLabelHopeless
  , pattern HKStateOfMindLabelIrritated
  , pattern HKStateOfMindLabelJealous
  , pattern HKStateOfMindLabelJoyful
  , pattern HKStateOfMindLabelLonely
  , pattern HKStateOfMindLabelPassionate
  , pattern HKStateOfMindLabelPeaceful
  , pattern HKStateOfMindLabelProud
  , pattern HKStateOfMindLabelRelieved
  , pattern HKStateOfMindLabelSad
  , pattern HKStateOfMindLabelScared
  , pattern HKStateOfMindLabelStressed
  , pattern HKStateOfMindLabelSurprised
  , pattern HKStateOfMindLabelWorried
  , pattern HKStateOfMindLabelAnnoyed
  , pattern HKStateOfMindLabelConfident
  , pattern HKStateOfMindLabelDrained
  , pattern HKStateOfMindLabelHopeful
  , pattern HKStateOfMindLabelIndifferent
  , pattern HKStateOfMindLabelOverwhelmed
  , pattern HKStateOfMindLabelSatisfied
  , HKWorkoutActivityType(HKWorkoutActivityType)
  , pattern HKWorkoutActivityTypeAmericanFootball
  , pattern HKWorkoutActivityTypeArchery
  , pattern HKWorkoutActivityTypeAustralianFootball
  , pattern HKWorkoutActivityTypeBadminton
  , pattern HKWorkoutActivityTypeBaseball
  , pattern HKWorkoutActivityTypeBasketball
  , pattern HKWorkoutActivityTypeBowling
  , pattern HKWorkoutActivityTypeBoxing
  , pattern HKWorkoutActivityTypeClimbing
  , pattern HKWorkoutActivityTypeCricket
  , pattern HKWorkoutActivityTypeCrossTraining
  , pattern HKWorkoutActivityTypeCurling
  , pattern HKWorkoutActivityTypeCycling
  , pattern HKWorkoutActivityTypeDance
  , pattern HKWorkoutActivityTypeDanceInspiredTraining
  , pattern HKWorkoutActivityTypeElliptical
  , pattern HKWorkoutActivityTypeEquestrianSports
  , pattern HKWorkoutActivityTypeFencing
  , pattern HKWorkoutActivityTypeFishing
  , pattern HKWorkoutActivityTypeFunctionalStrengthTraining
  , pattern HKWorkoutActivityTypeGolf
  , pattern HKWorkoutActivityTypeGymnastics
  , pattern HKWorkoutActivityTypeHandball
  , pattern HKWorkoutActivityTypeHiking
  , pattern HKWorkoutActivityTypeHockey
  , pattern HKWorkoutActivityTypeHunting
  , pattern HKWorkoutActivityTypeLacrosse
  , pattern HKWorkoutActivityTypeMartialArts
  , pattern HKWorkoutActivityTypeMindAndBody
  , pattern HKWorkoutActivityTypeMixedMetabolicCardioTraining
  , pattern HKWorkoutActivityTypePaddleSports
  , pattern HKWorkoutActivityTypePlay
  , pattern HKWorkoutActivityTypePreparationAndRecovery
  , pattern HKWorkoutActivityTypeRacquetball
  , pattern HKWorkoutActivityTypeRowing
  , pattern HKWorkoutActivityTypeRugby
  , pattern HKWorkoutActivityTypeRunning
  , pattern HKWorkoutActivityTypeSailing
  , pattern HKWorkoutActivityTypeSkatingSports
  , pattern HKWorkoutActivityTypeSnowSports
  , pattern HKWorkoutActivityTypeSoccer
  , pattern HKWorkoutActivityTypeSoftball
  , pattern HKWorkoutActivityTypeSquash
  , pattern HKWorkoutActivityTypeStairClimbing
  , pattern HKWorkoutActivityTypeSurfingSports
  , pattern HKWorkoutActivityTypeSwimming
  , pattern HKWorkoutActivityTypeTableTennis
  , pattern HKWorkoutActivityTypeTennis
  , pattern HKWorkoutActivityTypeTrackAndField
  , pattern HKWorkoutActivityTypeTraditionalStrengthTraining
  , pattern HKWorkoutActivityTypeVolleyball
  , pattern HKWorkoutActivityTypeWalking
  , pattern HKWorkoutActivityTypeWaterFitness
  , pattern HKWorkoutActivityTypeWaterPolo
  , pattern HKWorkoutActivityTypeWaterSports
  , pattern HKWorkoutActivityTypeWrestling
  , pattern HKWorkoutActivityTypeYoga
  , pattern HKWorkoutActivityTypeBarre
  , pattern HKWorkoutActivityTypeCoreTraining
  , pattern HKWorkoutActivityTypeCrossCountrySkiing
  , pattern HKWorkoutActivityTypeDownhillSkiing
  , pattern HKWorkoutActivityTypeFlexibility
  , pattern HKWorkoutActivityTypeHighIntensityIntervalTraining
  , pattern HKWorkoutActivityTypeJumpRope
  , pattern HKWorkoutActivityTypeKickboxing
  , pattern HKWorkoutActivityTypePilates
  , pattern HKWorkoutActivityTypeSnowboarding
  , pattern HKWorkoutActivityTypeStairs
  , pattern HKWorkoutActivityTypeStepTraining
  , pattern HKWorkoutActivityTypeWheelchairWalkPace
  , pattern HKWorkoutActivityTypeWheelchairRunPace
  , pattern HKWorkoutActivityTypeTaiChi
  , pattern HKWorkoutActivityTypeMixedCardio
  , pattern HKWorkoutActivityTypeHandCycling
  , pattern HKWorkoutActivityTypeDiscSports
  , pattern HKWorkoutActivityTypeFitnessGaming
  , pattern HKWorkoutActivityTypeCardioDance
  , pattern HKWorkoutActivityTypeSocialDance
  , pattern HKWorkoutActivityTypePickleball
  , pattern HKWorkoutActivityTypeCooldown
  , pattern HKWorkoutActivityTypeSwimBikeRun
  , pattern HKWorkoutActivityTypeTransition
  , pattern HKWorkoutActivityTypeUnderwaterDiving
  , pattern HKWorkoutActivityTypeOther
  , NSPredicateOperatorType(NSPredicateOperatorType)
  , pattern NSLessThanPredicateOperatorType
  , pattern NSLessThanOrEqualToPredicateOperatorType
  , pattern NSGreaterThanPredicateOperatorType
  , pattern NSGreaterThanOrEqualToPredicateOperatorType
  , pattern NSEqualToPredicateOperatorType
  , pattern NSNotEqualToPredicateOperatorType
  , pattern NSMatchesPredicateOperatorType
  , pattern NSLikePredicateOperatorType
  , pattern NSBeginsWithPredicateOperatorType
  , pattern NSEndsWithPredicateOperatorType
  , pattern NSInPredicateOperatorType
  , pattern NSCustomSelectorPredicateOperatorType
  , pattern NSContainsPredicateOperatorType
  , pattern NSBetweenPredicateOperatorType

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
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKQuery hkQuery => hkQuery -> IO (Id HKQuery)
init_ hkQuery  =
    sendMsg hkQuery (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | predicateForUserAnnotatedMedicationsWithIsArchived:
--
-- Creates a predicate for use with HKUserAnnotatedMedicationQuery.
--
-- Creates a query predicate that matches HKUserAnnotatedMedication objects that have the archived status specified.
--
-- @isArchived@ — The archived status of the medication. Ex: True will match medications in the archived section in the Health App.
--
-- ObjC selector: @+ predicateForUserAnnotatedMedicationsWithIsArchived:@
predicateForUserAnnotatedMedicationsWithIsArchived :: Bool -> IO (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithIsArchived isArchived =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForUserAnnotatedMedicationsWithIsArchived:") (retPtr retVoid) [argCULong (if isArchived then 1 else 0)] >>= retainedObject . castPtr

-- | predicateForUserAnnotatedMedicationsWithHasSchedule:
--
-- Creates a predicate for use with HKUserAnnotatedMedicationQuery.
--
-- Creates a query predicate that matches HKUserAnnotatedMedication objects that match the schedule status specified.
--
-- @hasSchedule@ — The schedule status of the medication. Ex: True will match medications that have a reminders schedule set up in the Health App.
--
-- ObjC selector: @+ predicateForUserAnnotatedMedicationsWithHasSchedule:@
predicateForUserAnnotatedMedicationsWithHasSchedule :: Bool -> IO (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithHasSchedule hasSchedule =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForUserAnnotatedMedicationsWithHasSchedule:") (retPtr retVoid) [argCULong (if hasSchedule then 1 else 0)] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithStatus:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have the status specified.
--
-- @status@ — The logged status of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithStatus:@
predicateForMedicationDoseEventWithStatus :: HKMedicationDoseEventLogStatus -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithStatus status =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithStatus:") (retPtr retVoid) [argCLong (coerce status)] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithStatuses:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have any of the statuses specified.
--
-- @statuses@ — The logged statuses of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithStatuses:@
predicateForMedicationDoseEventWithStatuses :: IsNSSet statuses => statuses -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithStatuses statuses =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr statuses $ \raw_statuses ->
      sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithStatuses:") (retPtr retVoid) [argPtr (castPtr raw_statuses :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithScheduledDate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have the exact scheduled date specified.
--
-- @scheduledDate@ — The exact scheduled date of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledDate:@
predicateForMedicationDoseEventWithScheduledDate :: IsNSDate scheduledDate => scheduledDate -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDate scheduledDate =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr scheduledDate $ \raw_scheduledDate ->
      sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithScheduledDate:") (retPtr retVoid) [argPtr (castPtr raw_scheduledDate :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithScheduledDates:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have any of the exact scheduled dates specified.
--
-- @scheduledDates@ — The exact scheduled dates of any medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledDates:@
predicateForMedicationDoseEventWithScheduledDates :: IsNSSet scheduledDates => scheduledDates -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDates scheduledDates =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr scheduledDates $ \raw_scheduledDates ->
      sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithScheduledDates:") (retPtr retVoid) [argPtr (castPtr raw_scheduledDates :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithScheduledStartDate:endDate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have a scheduled date within a window of scheduled times. If nil is provided to either parameter, the respective side of the window is unbound.
--
-- @startDate@ — The beginning of the window for scheduled dates of any medication dose event to match.
--
-- @endDate@ — The beginning of the window for scheduled dates of any medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledStartDate:endDate:@
predicateForMedicationDoseEventWithScheduledStartDate_endDate :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledStartDate_endDate startDate endDate =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr startDate $ \raw_startDate ->
      withObjCPtr endDate $ \raw_endDate ->
        sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithScheduledStartDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithMedicationConceptIdentifier:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that match a medication's concept identifier.
--
-- @medicationConceptIdentifier@ — The identifier of the medication that a dose event was created for.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithMedicationConceptIdentifier:@
predicateForMedicationDoseEventWithMedicationConceptIdentifier :: IsHKHealthConceptIdentifier medicationConceptIdentifier => medicationConceptIdentifier -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifier medicationConceptIdentifier =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr medicationConceptIdentifier $ \raw_medicationConceptIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_medicationConceptIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForMedicationDoseEventWithMedicationConceptIdentifiers:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples generated by any medication in a set of medication concept identifiers.
--
-- @medicationConceptIdentifiers@ — Any identifier of a medication that a dose event was created for.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithMedicationConceptIdentifiers:@
predicateForMedicationDoseEventWithMedicationConceptIdentifiers :: IsNSSet medicationConceptIdentifiers => medicationConceptIdentifiers -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifiers medicationConceptIdentifiers =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr medicationConceptIdentifiers $ \raw_medicationConceptIdentifiers ->
      sendClassMsg cls' (mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_medicationConceptIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForStatesOfMindWithValence:operatorType:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have a valence property matching the operator type and valence.
--
-- @valence@ — The value to be compared against.
--
-- @operatorType@ — The comparison operator type for the expression.
--
-- ObjC selector: @+ predicateForStatesOfMindWithValence:operatorType:@
predicateForStatesOfMindWithValence_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForStatesOfMindWithValence_operatorType valence operatorType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForStatesOfMindWithValence:operatorType:") (retPtr retVoid) [argCDouble valence, argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- | predicateForStatesOfMindWithKind:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified kind of feeling type.
--
-- @kind@ — The kind of feeling type to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithKind:@
predicateForStatesOfMindWithKind :: HKStateOfMindKind -> IO (Id NSPredicate)
predicateForStatesOfMindWithKind kind =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForStatesOfMindWithKind:") (retPtr retVoid) [argCLong (coerce kind)] >>= retainedObject . castPtr

-- | predicateForStatesOfMindWithLabel:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified label.
--
-- @label@ — The label to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithLabel:@
predicateForStatesOfMindWithLabel :: HKStateOfMindLabel -> IO (Id NSPredicate)
predicateForStatesOfMindWithLabel label =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForStatesOfMindWithLabel:") (retPtr retVoid) [argCLong (coerce label)] >>= retainedObject . castPtr

-- | predicateForStatesOfMindWithAssociation:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified association.
--
-- @association@ — The association to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithAssociation:@
predicateForStatesOfMindWithAssociation :: HKStateOfMindAssociation -> IO (Id NSPredicate)
predicateForStatesOfMindWithAssociation association =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForStatesOfMindWithAssociation:") (retPtr retVoid) [argCLong (coerce association)] >>= retainedObject . castPtr

-- | predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a predicate that matches HKVerifiableClinicalRecords with a relevant date within a date interval.
--
-- @dateInterval@ — The date interval that the record's relevant date is in.
--
-- ObjC selector: @+ predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:@
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval :: IsNSDateInterval dateInterval => dateInterval -> IO (Id NSPredicate)
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval dateInterval =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr dateInterval $ \raw_dateInterval ->
      sendClassMsg cls' (mkSelector "predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:") (retPtr retVoid) [argPtr (castPtr raw_dateInterval :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForElectrocardiogramsWithClassification:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKElectrocardiograms with a specific classification.
--
-- @classification@ — The classification for the electrocardiogram.
--
-- ObjC selector: @+ predicateForElectrocardiogramsWithClassification:@
predicateForElectrocardiogramsWithClassification :: HKElectrocardiogramClassification -> IO (Id NSPredicate)
predicateForElectrocardiogramsWithClassification classification =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForElectrocardiogramsWithClassification:") (retPtr retVoid) [argCLong (coerce classification)] >>= retainedObject . castPtr

-- | predicateForElectrocardiogramsWithSymptomsStatus:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKElectrocardiograms with a specificied symptoms status.
--
-- @symptomsStatus@ — The symptoms status for the electrocardiogram.
--
-- ObjC selector: @+ predicateForElectrocardiogramsWithSymptomsStatus:@
predicateForElectrocardiogramsWithSymptomsStatus :: HKElectrocardiogramSymptomsStatus -> IO (Id NSPredicate)
predicateForElectrocardiogramsWithSymptomsStatus symptomsStatus =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForElectrocardiogramsWithSymptomsStatus:") (retPtr retVoid) [argCLong (coerce symptomsStatus)] >>= retainedObject . castPtr

-- | predicateForClinicalRecordsWithFHIRResourceType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKClinicalRecords with a specific FHIR resource type.
--
-- @resourceType@ — The FHIR resource type.
--
-- ObjC selector: @+ predicateForClinicalRecordsWithFHIRResourceType:@
predicateForClinicalRecordsWithFHIRResourceType :: IsNSString resourceType => resourceType -> IO (Id NSPredicate)
predicateForClinicalRecordsWithFHIRResourceType resourceType =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr resourceType $ \raw_resourceType ->
      sendClassMsg cls' (mkSelector "predicateForClinicalRecordsWithFHIRResourceType:") (retPtr retVoid) [argPtr (castPtr raw_resourceType :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForClinicalRecordsFromSource:withFHIRResourceType:identifier:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKClinicalRecords for a given source, FHIR resource type, and FHIR identifier.
--
-- @source@ — The source.
--
-- @resourceType@ — The FHIR resource type.
--
-- @identifier@ — The FHIR identifier.
--
-- ObjC selector: @+ predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:@
predicateForClinicalRecordsFromSource_FHIRResourceType_identifier :: (IsHKSource source, IsNSString resourceType, IsNSString identifier) => source -> resourceType -> identifier -> IO (Id NSPredicate)
predicateForClinicalRecordsFromSource_FHIRResourceType_identifier source resourceType identifier =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr source $ \raw_source ->
      withObjCPtr resourceType $ \raw_resourceType ->
        withObjCPtr identifier $ \raw_identifier ->
          sendClassMsg cls' (mkSelector "predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_resourceType :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForActivitySummaryWithDateComponents:
--
-- Creates a predicate for use with HKActivitySummaryQuery
--
-- Creates a query predicate that matches HKActivitySummaries with the given date components.
--
-- @dateComponents@ — The date components of the activity summary. These date components should contain era, year, month,                and day components in the gregorian calendar.
--
-- ObjC selector: @+ predicateForActivitySummaryWithDateComponents:@
predicateForActivitySummaryWithDateComponents :: IsNSDateComponents dateComponents => dateComponents -> IO (Id NSPredicate)
predicateForActivitySummaryWithDateComponents dateComponents =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr dateComponents $ \raw_dateComponents ->
      sendClassMsg cls' (mkSelector "predicateForActivitySummaryWithDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_dateComponents :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:
--
-- Creates a predicate for use with HKActivitySummaryQuery
--
-- Creates a query predicate that matches HKActivitySummaries that fall between the given date components.
--
-- @startDateComponents@ — The date components that define the beginning of the range. These date components should contain                 era, year, month, and day components in the gregorian calendar.
--
-- @endDateComponents@ — The date components that define the end of the range. These date components should contain era,                 year, month, and day components in the gregorian calendar.
--
-- ObjC selector: @+ predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:@
predicateForActivitySummariesBetweenStartDateComponents_endDateComponents :: (IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => startDateComponents -> endDateComponents -> IO (Id NSPredicate)
predicateForActivitySummariesBetweenStartDateComponents_endDateComponents startDateComponents endDateComponents =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr startDateComponents $ \raw_startDateComponents ->
      withObjCPtr endDateComponents $ \raw_endDateComponents ->
        sendClassMsg cls' (mkSelector "predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_startDateComponents :: Ptr ()), argPtr (castPtr raw_endDateComponents :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithWorkoutActivityType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects with the given HKWorkoutActivityType.                The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate: before being used in a query.
--
-- @workoutActivityType@ — The HKWorkoutActivity type of the workout
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithWorkoutActivityType:@
predicateForWorkoutActivitiesWithWorkoutActivityType :: HKWorkoutActivityType -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithWorkoutActivityType workoutActivityType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithWorkoutActivityType:") (retPtr retVoid) [argCULong (coerce workoutActivityType)] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithOperatorType:duration:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects by the given operator type and duration.                The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate: before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @duration@ — The value that the workout's duration is being compared to. It is the right hand side of the                                expression.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:duration:@
predicateForWorkoutActivitiesWithOperatorType_duration :: NSPredicateOperatorType -> CDouble -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_duration operatorType duration =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithOperatorType:duration:") (retPtr retVoid) [argCULong (coerce operatorType), argCDouble duration] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithStartDate:endDate:options:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects with a startDate and an endDate that lie inside of a                given time interval. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @startDate@ — The start date of the predicate's time interval.
--
-- @endDate@ — The end date of the predicate's time interval.
--
-- @options@ — The rules for how a activity's time interval overlaps with the predicate's time interval.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithStartDate:endDate:options:@
predicateForWorkoutActivitiesWithStartDate_endDate_options :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> HKQueryOptions -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithStartDate_endDate_options startDate endDate options =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr startDate $ \raw_startDate ->
      withObjCPtr endDate $ \raw_endDate ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithStartDate:endDate:options:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and sumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a cumulative quantity type.
--
-- @sumQuantity@ — The sum value that the activity statistics are being compared to. The unit for this value should                                match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity sumQuantity) => NSPredicateOperatorType -> quantityType -> sumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity operatorType quantityType sumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr sumQuantity $ \raw_sumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_sumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs  by the given operator type and minimumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @minimumQuantity@ — The minumum value that the activty statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity minimumQuantity) => NSPredicateOperatorType -> quantityType -> minimumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity operatorType quantityType minimumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr minimumQuantity $ \raw_minimumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_minimumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and maximumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @maximumQuantity@ — The maximum value that the activity statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity maximumQuantity) => NSPredicateOperatorType -> quantityType -> maximumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity operatorType quantityType maximumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr maximumQuantity $ \raw_maximumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_maximumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and averageQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @averageQuantity@ — The average value that the activity statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity :: (IsHKQuantityType quantityType, IsHKQuantity averageQuantity) => NSPredicateOperatorType -> quantityType -> averageQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity operatorType quantityType averageQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr averageQuantity $ \raw_averageQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_averageQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithActivityPredicate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches workouts containing an activity matching the passed predicate.
--
-- @activityPredicate@ — The predicate on the activities of the workout
--
-- ObjC selector: @+ predicateForWorkoutsWithActivityPredicate:@
predicateForWorkoutsWithActivityPredicate :: IsNSPredicate activityPredicate => activityPredicate -> IO (Id NSPredicate)
predicateForWorkoutsWithActivityPredicate activityPredicate =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr activityPredicate $ \raw_activityPredicate ->
      sendClassMsg cls' (mkSelector "predicateForWorkoutsWithActivityPredicate:") (retPtr retVoid) [argPtr (castPtr raw_activityPredicate :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithWorkoutActivityType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts with the given HKWorkoutActivityType.
--
-- @workoutActivityType@ — The HKWorkoutActivity type of the workout
--
-- ObjC selector: @+ predicateForWorkoutsWithWorkoutActivityType:@
predicateForWorkoutsWithWorkoutActivityType :: HKWorkoutActivityType -> IO (Id NSPredicate)
predicateForWorkoutsWithWorkoutActivityType workoutActivityType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForWorkoutsWithWorkoutActivityType:") (retPtr retVoid) [argCULong (coerce workoutActivityType)] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:duration:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and duration
--
-- @operatorType@ — The operator type for the expression.
--
-- @duration@ — The value that the workout's duration is being compared to. It is the right hand side of the                                expression.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:duration:@
predicateForWorkoutsWithOperatorType_duration :: NSPredicateOperatorType -> CDouble -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_duration operatorType duration =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:duration:") (retPtr retVoid) [argCULong (coerce operatorType), argCDouble duration] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:totalEnergyBurned:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalEnergyBurned
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalEnergyBurned@ — The value that the workout's totalEnergyBurned is being compared to. It is the right hand side of the                                    expression. The unit for this value should be of type Energy.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalEnergyBurned:@
predicateForWorkoutsWithOperatorType_totalEnergyBurned :: IsHKQuantity totalEnergyBurned => NSPredicateOperatorType -> totalEnergyBurned -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalEnergyBurned operatorType totalEnergyBurned =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr totalEnergyBurned $ \raw_totalEnergyBurned ->
      sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:totalEnergyBurned:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_totalEnergyBurned :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:totalDistance:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalEnergyBurned
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalDistance@ — The value that the workout's totalEnergyBurned is being compared to. It is the right hand side of the                                expression. The unit for this value should be of type Distance.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalDistance:@
predicateForWorkoutsWithOperatorType_totalDistance :: IsHKQuantity totalDistance => NSPredicateOperatorType -> totalDistance -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalDistance operatorType totalDistance =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr totalDistance $ \raw_totalDistance ->
      sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:totalDistance:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_totalDistance :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalSwimmingStrokeCount
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalSwimmingStrokeCount@ — The value that the workout's totalSwimmingStrokeCount is being compared to.                                            It is the right hand side of the expression. The unit for this value should                                            be of type Count.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:@
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount :: IsHKQuantity totalSwimmingStrokeCount => NSPredicateOperatorType -> totalSwimmingStrokeCount -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount operatorType totalSwimmingStrokeCount =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr totalSwimmingStrokeCount $ \raw_totalSwimmingStrokeCount ->
      sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_totalSwimmingStrokeCount :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:totalFlightsClimbed:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalFlightsClimbed
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalFlightsClimbed@ — The value that the workout's totalFlightsClimbed is being compared to.                                            It is the right hand side of the expression. The unit for this value should                                            be of type Count.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalFlightsClimbed:@
predicateForWorkoutsWithOperatorType_totalFlightsClimbed :: IsHKQuantity totalFlightsClimbed => NSPredicateOperatorType -> totalFlightsClimbed -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalFlightsClimbed operatorType totalFlightsClimbed =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr totalFlightsClimbed $ \raw_totalFlightsClimbed ->
      sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:totalFlightsClimbed:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_totalFlightsClimbed :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and sumQuantity in the statistics for                the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a cumulative quantity type.
--
-- @sumQuantity@ — The sum value that the workout statistics are being compared to. The unit for this value should                                match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_sumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity sumQuantity) => NSPredicateOperatorType -> quantityType -> sumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_sumQuantity operatorType quantityType sumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr sumQuantity $ \raw_sumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_sumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and minimumQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @minimumQuantity@ — The minumum value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity minimumQuantity) => NSPredicateOperatorType -> quantityType -> minimumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity operatorType quantityType minimumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr minimumQuantity $ \raw_minimumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_minimumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and maximumQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @maximumQuantity@ — The maximum value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity maximumQuantity) => NSPredicateOperatorType -> quantityType -> maximumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity operatorType quantityType maximumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr maximumQuantity $ \raw_maximumQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_maximumQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and averageQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @averageQuantity@ — The average value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_averageQuantity :: (IsHKQuantityType quantityType, IsHKQuantity averageQuantity) => NSPredicateOperatorType -> quantityType -> averageQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_averageQuantity operatorType quantityType averageQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr averageQuantity $ \raw_averageQuantity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_averageQuantity :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForCategorySamplesWithOperatorType:value:@
predicateForCategorySamplesWithOperatorType_value :: NSPredicateOperatorType -> CLong -> IO (Id NSPredicate)
predicateForCategorySamplesWithOperatorType_value operatorType value =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForCategorySamplesWithOperatorType:value:") (retPtr retVoid) [argCULong (coerce operatorType), argCLong value] >>= retainedObject . castPtr

-- | predicateForCategorySamplesEqualToValues:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches all specified category values.
--
-- ObjC selector: @+ predicateForCategorySamplesEqualToValues:@
predicateForCategorySamplesEqualToValues :: IsNSSet values => values -> IO (Id NSPredicate)
predicateForCategorySamplesEqualToValues values =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr values $ \raw_values ->
      sendClassMsg cls' (mkSelector "predicateForCategorySamplesEqualToValues:") (retPtr retVoid) [argPtr (castPtr raw_values :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForQuantitySamplesWithOperatorType:quantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches quantity samples with values that match the expression formed by                the given operator and quantity.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantity@ — The quantity that the sample's quantity is being compared to. It is the right hand side                                of the expression.
--
-- ObjC selector: @+ predicateForQuantitySamplesWithOperatorType:quantity:@
predicateForQuantitySamplesWithOperatorType_quantity :: IsHKQuantity quantity => NSPredicateOperatorType -> quantity -> IO (Id NSPredicate)
predicateForQuantitySamplesWithOperatorType_quantity operatorType quantity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr quantity $ \raw_quantity ->
      sendClassMsg cls' (mkSelector "predicateForQuantitySamplesWithOperatorType:quantity:") (retPtr retVoid) [argCULong (coerce operatorType), argPtr (castPtr raw_quantity :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForSamplesWithStartDate:endDate:options:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches samples with a startDate and an endDate that lie inside of a                given time interval.
--
-- @startDate@ — The start date of the predicate's time interval.
--
-- @endDate@ — The end date of the predicate's time interval.
--
-- @options@ — The rules for how a sample's time interval overlaps with the predicate's time interval.
--
-- ObjC selector: @+ predicateForSamplesWithStartDate:endDate:options:@
predicateForSamplesWithStartDate_endDate_options :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> HKQueryOptions -> IO (Id NSPredicate)
predicateForSamplesWithStartDate_endDate_options startDate endDate options =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr startDate $ \raw_startDate ->
      withObjCPtr endDate $ \raw_endDate ->
        sendClassMsg cls' (mkSelector "predicateForSamplesWithStartDate:endDate:options:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | predicateForObjectsWithMetadataKey:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects with metadata that contains a given key.
--
-- @key@ — The metadata key.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:@
predicateForObjectsWithMetadataKey :: IsNSString key => key -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey key =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "predicateForObjectsWithMetadataKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsWithMetadataKey:allowedValues:
--
-- Creates a predicate for use with HKQuery subclasses
--
-- Creates a query predicate that matches objects with metadata containing a value the matches one of the                given values for the given key.
--
-- @key@ — The metadata key.
--
-- @allowedValues@ — The list of values that the metadata value can be equal to.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:allowedValues:@
predicateForObjectsWithMetadataKey_allowedValues :: (IsNSString key, IsNSArray allowedValues) => key -> allowedValues -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey_allowedValues key allowedValues =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr key $ \raw_key ->
      withObjCPtr allowedValues $ \raw_allowedValues ->
        sendClassMsg cls' (mkSelector "predicateForObjectsWithMetadataKey:allowedValues:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_allowedValues :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsWithMetadataKey:operatorType:value:
--
-- Creates a predicate for use with HKQuery subclasses
--
-- Creates a query predicate that matches objects with a value for a given metadata key matches the given                operator type and value.
--
-- @key@ — The metadata key.
--
-- @operatorType@ — The comparison operator type for the expression.
--
-- @value@ — The value to be compared against.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:operatorType:value:@
predicateForObjectsWithMetadataKey_operatorType_value :: IsNSString key => key -> NSPredicateOperatorType -> RawId -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey_operatorType_value key operatorType value =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "predicateForObjectsWithMetadataKey:operatorType:value:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (coerce operatorType), argPtr (castPtr (unRawId value) :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsFromSource:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by a given source.
--
-- @source@ — The source.
--
-- ObjC selector: @+ predicateForObjectsFromSource:@
predicateForObjectsFromSource :: IsHKSource source => source -> IO (Id NSPredicate)
predicateForObjectsFromSource source =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "predicateForObjectsFromSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsFromSources:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by any of the given sources.
--
-- @sources@ — The list of sources.
--
-- ObjC selector: @+ predicateForObjectsFromSources:@
predicateForObjectsFromSources :: IsNSSet sources => sources -> IO (Id NSPredicate)
predicateForObjectsFromSources sources =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr sources $ \raw_sources ->
      sendClassMsg cls' (mkSelector "predicateForObjectsFromSources:") (retPtr retVoid) [argPtr (castPtr raw_sources :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsFromSourceRevisions:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by any of the specified HKSourceRevisions.
--
-- @sourceRevisions@ — The list of source revisions.
--
-- ObjC selector: @+ predicateForObjectsFromSourceRevisions:@
predicateForObjectsFromSourceRevisions :: IsNSSet sourceRevisions => sourceRevisions -> IO (Id NSPredicate)
predicateForObjectsFromSourceRevisions sourceRevisions =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr sourceRevisions $ \raw_sourceRevisions ->
      sendClassMsg cls' (mkSelector "predicateForObjectsFromSourceRevisions:") (retPtr retVoid) [argPtr (castPtr raw_sourceRevisions :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsFromDevices:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects associated with any of the given devices. All properties                of each HKDevice are considered in the query and must match exactly, including nil values. To perform                 searches based on specific device properties, use predicateForObjectsWithDeviceProperty:allowedValues:.
--
-- @devices@ — The set of devices that generated data.
--
-- ObjC selector: @+ predicateForObjectsFromDevices:@
predicateForObjectsFromDevices :: IsNSSet devices => devices -> IO (Id NSPredicate)
predicateForObjectsFromDevices devices =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr devices $ \raw_devices ->
      sendClassMsg cls' (mkSelector "predicateForObjectsFromDevices:") (retPtr retVoid) [argPtr (castPtr raw_devices :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsWithDeviceProperty:allowedValues:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects associated with an HKDevice with the specified device                property matching any value included in allowedValues. To query for samples with devices that match all                 the properties of an HKDevice, use predicateForObjectsFromDevices.
--
-- @key@ — The device property key. (See HKDevice.h)
--
-- @allowedValues@ — The set of values for which the device property can match. An empty set will match all                devices whose property value is nil.
--
-- ObjC selector: @+ predicateForObjectsWithDeviceProperty:allowedValues:@
predicateForObjectsWithDeviceProperty_allowedValues :: (IsNSString key, IsNSSet allowedValues) => key -> allowedValues -> IO (Id NSPredicate)
predicateForObjectsWithDeviceProperty_allowedValues key allowedValues =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr key $ \raw_key ->
      withObjCPtr allowedValues $ \raw_allowedValues ->
        sendClassMsg cls' (mkSelector "predicateForObjectsWithDeviceProperty:allowedValues:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_allowedValues :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectWithUUID:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the object saved with a particular UUID.
--
-- @UUID@ — The UUID of the object.
--
-- ObjC selector: @+ predicateForObjectWithUUID:@
predicateForObjectWithUUID :: IsNSUUID uuid => uuid -> IO (Id NSPredicate)
predicateForObjectWithUUID uuid =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr uuid $ \raw_uuid ->
      sendClassMsg cls' (mkSelector "predicateForObjectWithUUID:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsWithUUIDs:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects saved with one of the given UUIDs.
--
-- @UUIDs@ — The set of NSUUIDs.
--
-- ObjC selector: @+ predicateForObjectsWithUUIDs:@
predicateForObjectsWithUUIDs :: IsNSSet uuiDs => uuiDs -> IO (Id NSPredicate)
predicateForObjectsWithUUIDs uuiDs =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr uuiDs $ \raw_uuiDs ->
      sendClassMsg cls' (mkSelector "predicateForObjectsWithUUIDs:") (retPtr retVoid) [argPtr (castPtr raw_uuiDs :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsNoCorrelation
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that are not associated with an HKCorrelation.
--
-- ObjC selector: @+ predicateForObjectsWithNoCorrelation@
predicateForObjectsWithNoCorrelation :: IO (Id NSPredicate)
predicateForObjectsWithNoCorrelation  =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMsg cls' (mkSelector "predicateForObjectsWithNoCorrelation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | predicateForObjectsFromWorkout:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that have been added to the given workout.
--
-- @workout@ — The HKWorkout that the object was added to.
--
-- ObjC selector: @+ predicateForObjectsFromWorkout:@
predicateForObjectsFromWorkout :: IsHKWorkout workout => workout -> IO (Id NSPredicate)
predicateForObjectsFromWorkout workout =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr workout $ \raw_workout ->
      sendClassMsg cls' (mkSelector "predicateForObjectsFromWorkout:") (retPtr retVoid) [argPtr (castPtr raw_workout :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForObjectsAssociatedWithElectrocardiogram:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that have been added to the given electrocardiogram
--
-- @electrocardiogram@ — The HKElectrocardiogram that the object was added to.
--
-- ObjC selector: @+ predicateForObjectsAssociatedWithElectrocardiogram:@
predicateForObjectsAssociatedWithElectrocardiogram :: IsHKElectrocardiogram electrocardiogram => electrocardiogram -> IO (Id NSPredicate)
predicateForObjectsAssociatedWithElectrocardiogram electrocardiogram =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr electrocardiogram $ \raw_electrocardiogram ->
      sendClassMsg cls' (mkSelector "predicateForObjectsAssociatedWithElectrocardiogram:") (retPtr retVoid) [argPtr (castPtr raw_electrocardiogram :: Ptr ())] >>= retainedObject . castPtr

-- | predicateForWorkoutEffortSamplesRelatedToWorkout:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches Workout Effort samples that have been related to the given workout
--
-- @workout@ — The HKWorkout that the object is related to.
--
-- @activity@ — The HKWorkoutActivity that the object is related to.
--
-- ObjC selector: @+ predicateForWorkoutEffortSamplesRelatedToWorkout:activity:@
predicateForWorkoutEffortSamplesRelatedToWorkout_activity :: (IsHKWorkout workout, IsHKWorkoutActivity activity) => workout -> activity -> IO (Id NSPredicate)
predicateForWorkoutEffortSamplesRelatedToWorkout_activity workout activity =
  do
    cls' <- getRequiredClass "HKQuery"
    withObjCPtr workout $ \raw_workout ->
      withObjCPtr activity $ \raw_activity ->
        sendClassMsg cls' (mkSelector "predicateForWorkoutEffortSamplesRelatedToWorkout:activity:") (retPtr retVoid) [argPtr (castPtr raw_workout :: Ptr ()), argPtr (castPtr raw_activity :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectType@
objectType :: IsHKQuery hkQuery => hkQuery -> IO (Id HKObjectType)
objectType hkQuery  =
    sendMsg hkQuery (mkSelector "objectType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sampleType@
sampleType :: IsHKQuery hkQuery => hkQuery -> IO (Id HKSampleType)
sampleType hkQuery  =
    sendMsg hkQuery (mkSelector "sampleType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- predicate@
predicate :: IsHKQuery hkQuery => hkQuery -> IO (Id NSPredicate)
predicate hkQuery  =
    sendMsg hkQuery (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @predicateForUserAnnotatedMedicationsWithIsArchived:@
predicateForUserAnnotatedMedicationsWithIsArchivedSelector :: Selector
predicateForUserAnnotatedMedicationsWithIsArchivedSelector = mkSelector "predicateForUserAnnotatedMedicationsWithIsArchived:"

-- | @Selector@ for @predicateForUserAnnotatedMedicationsWithHasSchedule:@
predicateForUserAnnotatedMedicationsWithHasScheduleSelector :: Selector
predicateForUserAnnotatedMedicationsWithHasScheduleSelector = mkSelector "predicateForUserAnnotatedMedicationsWithHasSchedule:"

-- | @Selector@ for @predicateForMedicationDoseEventWithStatus:@
predicateForMedicationDoseEventWithStatusSelector :: Selector
predicateForMedicationDoseEventWithStatusSelector = mkSelector "predicateForMedicationDoseEventWithStatus:"

-- | @Selector@ for @predicateForMedicationDoseEventWithStatuses:@
predicateForMedicationDoseEventWithStatusesSelector :: Selector
predicateForMedicationDoseEventWithStatusesSelector = mkSelector "predicateForMedicationDoseEventWithStatuses:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledDate:@
predicateForMedicationDoseEventWithScheduledDateSelector :: Selector
predicateForMedicationDoseEventWithScheduledDateSelector = mkSelector "predicateForMedicationDoseEventWithScheduledDate:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledDates:@
predicateForMedicationDoseEventWithScheduledDatesSelector :: Selector
predicateForMedicationDoseEventWithScheduledDatesSelector = mkSelector "predicateForMedicationDoseEventWithScheduledDates:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledStartDate:endDate:@
predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector :: Selector
predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector = mkSelector "predicateForMedicationDoseEventWithScheduledStartDate:endDate:"

-- | @Selector@ for @predicateForMedicationDoseEventWithMedicationConceptIdentifier:@
predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector :: Selector
predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector = mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifier:"

-- | @Selector@ for @predicateForMedicationDoseEventWithMedicationConceptIdentifiers:@
predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector :: Selector
predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector = mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifiers:"

-- | @Selector@ for @predicateForStatesOfMindWithValence:operatorType:@
predicateForStatesOfMindWithValence_operatorTypeSelector :: Selector
predicateForStatesOfMindWithValence_operatorTypeSelector = mkSelector "predicateForStatesOfMindWithValence:operatorType:"

-- | @Selector@ for @predicateForStatesOfMindWithKind:@
predicateForStatesOfMindWithKindSelector :: Selector
predicateForStatesOfMindWithKindSelector = mkSelector "predicateForStatesOfMindWithKind:"

-- | @Selector@ for @predicateForStatesOfMindWithLabel:@
predicateForStatesOfMindWithLabelSelector :: Selector
predicateForStatesOfMindWithLabelSelector = mkSelector "predicateForStatesOfMindWithLabel:"

-- | @Selector@ for @predicateForStatesOfMindWithAssociation:@
predicateForStatesOfMindWithAssociationSelector :: Selector
predicateForStatesOfMindWithAssociationSelector = mkSelector "predicateForStatesOfMindWithAssociation:"

-- | @Selector@ for @predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:@
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector :: Selector
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector = mkSelector "predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:"

-- | @Selector@ for @predicateForElectrocardiogramsWithClassification:@
predicateForElectrocardiogramsWithClassificationSelector :: Selector
predicateForElectrocardiogramsWithClassificationSelector = mkSelector "predicateForElectrocardiogramsWithClassification:"

-- | @Selector@ for @predicateForElectrocardiogramsWithSymptomsStatus:@
predicateForElectrocardiogramsWithSymptomsStatusSelector :: Selector
predicateForElectrocardiogramsWithSymptomsStatusSelector = mkSelector "predicateForElectrocardiogramsWithSymptomsStatus:"

-- | @Selector@ for @predicateForClinicalRecordsWithFHIRResourceType:@
predicateForClinicalRecordsWithFHIRResourceTypeSelector :: Selector
predicateForClinicalRecordsWithFHIRResourceTypeSelector = mkSelector "predicateForClinicalRecordsWithFHIRResourceType:"

-- | @Selector@ for @predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:@
predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector :: Selector
predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector = mkSelector "predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:"

-- | @Selector@ for @predicateForActivitySummaryWithDateComponents:@
predicateForActivitySummaryWithDateComponentsSelector :: Selector
predicateForActivitySummaryWithDateComponentsSelector = mkSelector "predicateForActivitySummaryWithDateComponents:"

-- | @Selector@ for @predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:@
predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector :: Selector
predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector = mkSelector "predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithWorkoutActivityType:@
predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector :: Selector
predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector = mkSelector "predicateForWorkoutActivitiesWithWorkoutActivityType:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:duration:@
predicateForWorkoutActivitiesWithOperatorType_durationSelector :: Selector
predicateForWorkoutActivitiesWithOperatorType_durationSelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:duration:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithStartDate:endDate:options:@
predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector :: Selector
predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector = mkSelector "predicateForWorkoutActivitiesWithStartDate:endDate:options:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector :: Selector
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector :: Selector
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector :: Selector
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector :: Selector
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithActivityPredicate:@
predicateForWorkoutsWithActivityPredicateSelector :: Selector
predicateForWorkoutsWithActivityPredicateSelector = mkSelector "predicateForWorkoutsWithActivityPredicate:"

-- | @Selector@ for @predicateForWorkoutsWithWorkoutActivityType:@
predicateForWorkoutsWithWorkoutActivityTypeSelector :: Selector
predicateForWorkoutsWithWorkoutActivityTypeSelector = mkSelector "predicateForWorkoutsWithWorkoutActivityType:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:duration:@
predicateForWorkoutsWithOperatorType_durationSelector :: Selector
predicateForWorkoutsWithOperatorType_durationSelector = mkSelector "predicateForWorkoutsWithOperatorType:duration:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalEnergyBurned:@
predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector :: Selector
predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalEnergyBurned:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalDistance:@
predicateForWorkoutsWithOperatorType_totalDistanceSelector :: Selector
predicateForWorkoutsWithOperatorType_totalDistanceSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalDistance:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:@
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector :: Selector
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalFlightsClimbed:@
predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector :: Selector
predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalFlightsClimbed:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector :: Selector
predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector :: Selector
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector :: Selector
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector :: Selector
predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:"

-- | @Selector@ for @predicateForCategorySamplesWithOperatorType:value:@
predicateForCategorySamplesWithOperatorType_valueSelector :: Selector
predicateForCategorySamplesWithOperatorType_valueSelector = mkSelector "predicateForCategorySamplesWithOperatorType:value:"

-- | @Selector@ for @predicateForCategorySamplesEqualToValues:@
predicateForCategorySamplesEqualToValuesSelector :: Selector
predicateForCategorySamplesEqualToValuesSelector = mkSelector "predicateForCategorySamplesEqualToValues:"

-- | @Selector@ for @predicateForQuantitySamplesWithOperatorType:quantity:@
predicateForQuantitySamplesWithOperatorType_quantitySelector :: Selector
predicateForQuantitySamplesWithOperatorType_quantitySelector = mkSelector "predicateForQuantitySamplesWithOperatorType:quantity:"

-- | @Selector@ for @predicateForSamplesWithStartDate:endDate:options:@
predicateForSamplesWithStartDate_endDate_optionsSelector :: Selector
predicateForSamplesWithStartDate_endDate_optionsSelector = mkSelector "predicateForSamplesWithStartDate:endDate:options:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:@
predicateForObjectsWithMetadataKeySelector :: Selector
predicateForObjectsWithMetadataKeySelector = mkSelector "predicateForObjectsWithMetadataKey:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:allowedValues:@
predicateForObjectsWithMetadataKey_allowedValuesSelector :: Selector
predicateForObjectsWithMetadataKey_allowedValuesSelector = mkSelector "predicateForObjectsWithMetadataKey:allowedValues:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:operatorType:value:@
predicateForObjectsWithMetadataKey_operatorType_valueSelector :: Selector
predicateForObjectsWithMetadataKey_operatorType_valueSelector = mkSelector "predicateForObjectsWithMetadataKey:operatorType:value:"

-- | @Selector@ for @predicateForObjectsFromSource:@
predicateForObjectsFromSourceSelector :: Selector
predicateForObjectsFromSourceSelector = mkSelector "predicateForObjectsFromSource:"

-- | @Selector@ for @predicateForObjectsFromSources:@
predicateForObjectsFromSourcesSelector :: Selector
predicateForObjectsFromSourcesSelector = mkSelector "predicateForObjectsFromSources:"

-- | @Selector@ for @predicateForObjectsFromSourceRevisions:@
predicateForObjectsFromSourceRevisionsSelector :: Selector
predicateForObjectsFromSourceRevisionsSelector = mkSelector "predicateForObjectsFromSourceRevisions:"

-- | @Selector@ for @predicateForObjectsFromDevices:@
predicateForObjectsFromDevicesSelector :: Selector
predicateForObjectsFromDevicesSelector = mkSelector "predicateForObjectsFromDevices:"

-- | @Selector@ for @predicateForObjectsWithDeviceProperty:allowedValues:@
predicateForObjectsWithDeviceProperty_allowedValuesSelector :: Selector
predicateForObjectsWithDeviceProperty_allowedValuesSelector = mkSelector "predicateForObjectsWithDeviceProperty:allowedValues:"

-- | @Selector@ for @predicateForObjectWithUUID:@
predicateForObjectWithUUIDSelector :: Selector
predicateForObjectWithUUIDSelector = mkSelector "predicateForObjectWithUUID:"

-- | @Selector@ for @predicateForObjectsWithUUIDs:@
predicateForObjectsWithUUIDsSelector :: Selector
predicateForObjectsWithUUIDsSelector = mkSelector "predicateForObjectsWithUUIDs:"

-- | @Selector@ for @predicateForObjectsWithNoCorrelation@
predicateForObjectsWithNoCorrelationSelector :: Selector
predicateForObjectsWithNoCorrelationSelector = mkSelector "predicateForObjectsWithNoCorrelation"

-- | @Selector@ for @predicateForObjectsFromWorkout:@
predicateForObjectsFromWorkoutSelector :: Selector
predicateForObjectsFromWorkoutSelector = mkSelector "predicateForObjectsFromWorkout:"

-- | @Selector@ for @predicateForObjectsAssociatedWithElectrocardiogram:@
predicateForObjectsAssociatedWithElectrocardiogramSelector :: Selector
predicateForObjectsAssociatedWithElectrocardiogramSelector = mkSelector "predicateForObjectsAssociatedWithElectrocardiogram:"

-- | @Selector@ for @predicateForWorkoutEffortSamplesRelatedToWorkout:activity:@
predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector :: Selector
predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector = mkSelector "predicateForWorkoutEffortSamplesRelatedToWorkout:activity:"

-- | @Selector@ for @objectType@
objectTypeSelector :: Selector
objectTypeSelector = mkSelector "objectType"

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

