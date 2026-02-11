{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutBuilder
--
-- An HKWorkoutBuilder is used to incrementally create new workouts in the HealthKit database. Samples,                events, and metadata may be added to a builder either during a live workout session or to create a                workout that occurred in the past. Calling finishWorkoutWithCompletion: will create a new workout                with samples, events, and metadata that have been provided.
--
-- Generated bindings for @HKWorkoutBuilder@.
module ObjC.HealthKit.HKWorkoutBuilder
  ( HKWorkoutBuilder
  , IsHKWorkoutBuilder(..)
  , init_
  , initWithHealthStore_configuration_device
  , beginCollectionWithStartDate_completion
  , addSamples_completion
  , addWorkoutEvents_completion
  , addMetadata_completion
  , addWorkoutActivity_completion
  , updateActivityWithUUID_endDate_completion
  , updateActivityWithUUID_addMedatata_completion
  , endCollectionWithEndDate_completion
  , finishWorkoutWithCompletion
  , discardWorkout
  , elapsedTimeAtDate
  , statisticsForType
  , seriesBuilderForType
  , device
  , startDate
  , endDate
  , workoutConfiguration
  , metadata
  , workoutEvents
  , workoutActivities
  , allStatistics
  , initSelector
  , initWithHealthStore_configuration_deviceSelector
  , beginCollectionWithStartDate_completionSelector
  , addSamples_completionSelector
  , addWorkoutEvents_completionSelector
  , addMetadata_completionSelector
  , addWorkoutActivity_completionSelector
  , updateActivityWithUUID_endDate_completionSelector
  , updateActivityWithUUID_addMedatata_completionSelector
  , endCollectionWithEndDate_completionSelector
  , finishWorkoutWithCompletionSelector
  , discardWorkoutSelector
  , elapsedTimeAtDateSelector
  , statisticsForTypeSelector
  , seriesBuilderForTypeSelector
  , deviceSelector
  , startDateSelector
  , endDateSelector
  , workoutConfigurationSelector
  , metadataSelector
  , workoutEventsSelector
  , workoutActivitiesSelector
  , allStatisticsSelector


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
init_ :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id HKWorkoutBuilder)
init_ hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithHealthStore:configuration:device:
--
-- The designated initializer to create an HKWorkoutBuilder.
--
-- Creates a new HKWorkoutBuilder unconnected to any HKWorkoutSession or any sources of data.
--
-- @healthStore@ — Specifies the HKHealthStore object to use for building the workout. The store is retained                                until the builder is finished and a workout has been saved or discarded.
--
-- @configuration@ — The workout configuration to be used.
--
-- @device@ — The HKDevice to attach to the resulting HKWorkout.
--
-- ObjC selector: @- initWithHealthStore:configuration:device:@
initWithHealthStore_configuration_device :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsHKHealthStore healthStore, IsHKWorkoutConfiguration configuration, IsHKDevice device) => hkWorkoutBuilder -> healthStore -> configuration -> device -> IO (Id HKWorkoutBuilder)
initWithHealthStore_configuration_device hkWorkoutBuilder  healthStore configuration device =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr device $ \raw_device ->
          sendMsg hkWorkoutBuilder (mkSelector "initWithHealthStore:configuration:device:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | beginCollectionWithStartDate:error:
--
-- Sets the workout start date and activates the workout builder.
--
-- Calling this method is required before any samples, events or metadata can be added to the builder.
--
-- @startDate@ — The start date of the workout.
--
-- @completion@ — Called once data collection has started or has failed to start.
--
-- ObjC selector: @- beginCollectionWithStartDate:completion:@
beginCollectionWithStartDate_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSDate startDate) => hkWorkoutBuilder -> startDate -> Ptr () -> IO ()
beginCollectionWithStartDate_completion hkWorkoutBuilder  startDate completion =
  withObjCPtr startDate $ \raw_startDate ->
      sendMsg hkWorkoutBuilder (mkSelector "beginCollectionWithStartDate:completion:") retVoid [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | addSamples:completion:
--
-- Adds new samples to the builder instance. This method can be called multiple times to add samples                incrementally to the builder. The samples will be saved to the database if they have not already been                saved. The constraints of -[HKHealthStore saveObject:withCompletion:] apply to this method as well.                The start date of the samples must be later than the start date of the receiver. It is an error to call                this method after finishWorkoutWithCompletion: has been called. This operation is performed                asynchronously and the completion will be executed on an arbitrary background queue.
--
-- @samples@ — The samples to add to the workout.
--
-- @completion@ — Block to be called when the insertion is complete. If success is YES, the samples were added                            to the builder successfully. If success is NO, error will be non-nil and contain the error                            encountered while adding the new samples.
--
-- ObjC selector: @- addSamples:completion:@
addSamples_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSArray samples) => hkWorkoutBuilder -> samples -> Ptr () -> IO ()
addSamples_completion hkWorkoutBuilder  samples completion =
  withObjCPtr samples $ \raw_samples ->
      sendMsg hkWorkoutBuilder (mkSelector "addSamples:completion:") retVoid [argPtr (castPtr raw_samples :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | addWorkoutEvents:completion:
--
-- Adds new workout events to the builder instance. This method can be called many times to add workout                events incrementally to the builder. It is an error to call this method after                finishWorkoutWithCompletion: has been called. This operation is performed asynchronously and the                completion will be executed on an arbitrary background queue.
--
-- @workoutEvents@ — The events to add to the builder.
--
-- @completion@ — Block to be called when the addition of events to the builder is complete. If success is                                YES, the events were added to the builder successfully. If success is NO, error will be                                non-null and will contain the error encountered during the insertion operation.
--
-- ObjC selector: @- addWorkoutEvents:completion:@
addWorkoutEvents_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSArray workoutEvents) => hkWorkoutBuilder -> workoutEvents -> Ptr () -> IO ()
addWorkoutEvents_completion hkWorkoutBuilder  workoutEvents completion =
  withObjCPtr workoutEvents $ \raw_workoutEvents ->
      sendMsg hkWorkoutBuilder (mkSelector "addWorkoutEvents:completion:") retVoid [argPtr (castPtr raw_workoutEvents :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | addMetadata:completion:
--
-- Adds new metadata to the builder instance. This method can be called more than once; each time the newly                provided metadata will be merged with previously added metadata in the same manner as                -[NSMutableDictionary addEntriesFromDictionary:]. This operation is performed asynchronously and the                completion will be executed on an arbitrary background queue.
--
-- @metadata@ — The metadata to add to the workout.
--
-- @completion@ — Block to be called when the addition of metadata to the builder is complete. If success is                            YES, the metadata has been added to the builder successfully. If success is NO, error will                            be non-null and will contain the error encountered during the insertion operation. When an                            error occurs, the builder's metadata property will remain unchanged.
--
-- ObjC selector: @- addMetadata:completion:@
addMetadata_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSDictionary metadata) => hkWorkoutBuilder -> metadata -> Ptr () -> IO ()
addMetadata_completion hkWorkoutBuilder  metadata completion =
  withObjCPtr metadata $ \raw_metadata ->
      sendMsg hkWorkoutBuilder (mkSelector "addMetadata:completion:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | addWorkoutActivity:completion:
--
-- Adds a new workout activity to the builder instance. This method can be called many times to add workout                activities incrementally to the builder. It is an error to call this method after                finishWorkoutWithCompletion: has been called. This operation is performed asynchronously and the                completion will be executed on an arbitrary background queue.
--
-- @workoutActivity@ — The activity to add to the builder.
--
-- @completion@ — Block to be called when the addition of the activity to the builder is complete. If success is                                    YES, the activity was added to the builder successfully. If success is NO, error will be                                    non-null and will contain the error encountered during the insertion operation.
--
-- ObjC selector: @- addWorkoutActivity:completion:@
addWorkoutActivity_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsHKWorkoutActivity workoutActivity) => hkWorkoutBuilder -> workoutActivity -> Ptr () -> IO ()
addWorkoutActivity_completion hkWorkoutBuilder  workoutActivity completion =
  withObjCPtr workoutActivity $ \raw_workoutActivity ->
      sendMsg hkWorkoutBuilder (mkSelector "addWorkoutActivity:completion:") retVoid [argPtr (castPtr raw_workoutActivity :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | updateActivityWithUUID:endDate:completion:
--
-- Sets the end date on an already added activity. It is an error to call this method after                finishWorkoutWithCompletion: has been called. This operation is performed asynchronously and the                completion will be executed on an arbitrary background queue.
--
-- @UUID@ — The UUID of the workout activity to update.
--
-- @endDate@ — The end date to set on the activity
--
-- @completion@ — Block to be called when the update of the end date on the activity is complete. If success is                            YES, the end date was set to the actvity successfully. If success is NO, error will be                            non-null and will contain the error encountered during the update operation.
--
-- ObjC selector: @- updateActivityWithUUID:endDate:completion:@
updateActivityWithUUID_endDate_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSUUID uuid, IsNSDate endDate) => hkWorkoutBuilder -> uuid -> endDate -> Ptr () -> IO ()
updateActivityWithUUID_endDate_completion hkWorkoutBuilder  uuid endDate completion =
  withObjCPtr uuid $ \raw_uuid ->
    withObjCPtr endDate $ \raw_endDate ->
        sendMsg hkWorkoutBuilder (mkSelector "updateActivityWithUUID:endDate:completion:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | updateActivityWithUUID:addMetadata:completion:
--
-- Adds new metadata to an already added activity. This method can be called more than once; each time                the newly provided metadata will be merged with previously added metadata in the same manner as                -[NSMutableDictionary addEntriesFromDictionary:]. It is an error to call this method after                finishWorkoutWithCompletion: has been called. This operation is performed asynchronously and the                completion will be executed on an arbitrary background queue.
--
-- @UUID@ — The UUID of the workout activity to update.
--
-- @metadata@ — The metadata to add to the workout activity.
--
-- @completion@ — Block to be called when the addition of metadata to the activity is complete. If success is                            YES, the metadata has been added to the activity successfully. If success is NO, error will                            be non-null and will contain the error encountered during the insertion operation. When an                            error occurs, the activity's metadata property will remain unchanged.
--
-- ObjC selector: @- updateActivityWithUUID:addMedatata:completion:@
updateActivityWithUUID_addMedatata_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSUUID uuid, IsNSDictionary metadata) => hkWorkoutBuilder -> uuid -> metadata -> Ptr () -> IO ()
updateActivityWithUUID_addMedatata_completion hkWorkoutBuilder  uuid metadata completion =
  withObjCPtr uuid $ \raw_uuid ->
    withObjCPtr metadata $ \raw_metadata ->
        sendMsg hkWorkoutBuilder (mkSelector "updateActivityWithUUID:addMedatata:completion:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | endCollectionWithEndDate:error:
--
-- Sets the workout end date and deactivates the workout builer.
--
-- Calling this method is required before you finish a workout builder.
--
-- @endDate@ — The end date of the workout.
--
-- @completion@ — Called once data collection has stopped or has failed to stop.
--
-- ObjC selector: @- endCollectionWithEndDate:completion:@
endCollectionWithEndDate_completion :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSDate endDate) => hkWorkoutBuilder -> endDate -> Ptr () -> IO ()
endCollectionWithEndDate_completion hkWorkoutBuilder  endDate completion =
  withObjCPtr endDate $ \raw_endDate ->
      sendMsg hkWorkoutBuilder (mkSelector "endCollectionWithEndDate:completion:") retVoid [argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | finishWorkoutWithCompletion:
--
-- Creates and saves an HKWorkout using samples and events that have been added to workout previously.
--
-- @completion@ — Block to be called after the HKWorkout object has been created and saved. If the returned                            workout is nil, an error may have occurred in which case error will be non-nil. If both                            workout and error are nil then finishing the workout succeeded but the workout sample                            is not available because the device is locked.
--
-- ObjC selector: @- finishWorkoutWithCompletion:@
finishWorkoutWithCompletion :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> Ptr () -> IO ()
finishWorkoutWithCompletion hkWorkoutBuilder  completion =
    sendMsg hkWorkoutBuilder (mkSelector "finishWorkoutWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | discardWorkout
--
-- Finishes building the workout and discards the result instead of saving it. Samples that were added to                the workout will not be deleted. Adding samples, events, and metadata to the receiver after                discardWorkout has been called is an error.
--
-- ObjC selector: @- discardWorkout@
discardWorkout :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO ()
discardWorkout hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "discardWorkout") retVoid []

-- | elapsedTimeAtDate:
--
-- The elapsed duration of the workout evaluated at the specified date. The duration does not include                periods when the workout was paused, which are the intervals between pause and resume events.
--
-- ObjC selector: @- elapsedTimeAtDate:@
elapsedTimeAtDate :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsNSDate date) => hkWorkoutBuilder -> date -> IO CDouble
elapsedTimeAtDate hkWorkoutBuilder  date =
  withObjCPtr date $ \raw_date ->
      sendMsg hkWorkoutBuilder (mkSelector "elapsedTimeAtDate:") retCDouble [argPtr (castPtr raw_date :: Ptr ())]

-- | statisticsForType:
--
-- Returns an HKStatistics object containing the statistics for all the samples of the given type that                have been added to the receiver. If there are no samples of the given type then nil is returned.
--
-- @quantityType@ — The quantity type to gather statistics about.
--
-- ObjC selector: @- statisticsForType:@
statisticsForType :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsHKQuantityType quantityType) => hkWorkoutBuilder -> quantityType -> IO (Id HKStatistics)
statisticsForType hkWorkoutBuilder  quantityType =
  withObjCPtr quantityType $ \raw_quantityType ->
      sendMsg hkWorkoutBuilder (mkSelector "statisticsForType:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ())] >>= retainedObject . castPtr

-- | seriesBuilderForType:
--
-- Retrieves the associated series builder for the specified type.
--
-- Retrieves, and creates if it does not already exist, the series builder for the specified type. The                series constructed with the returned builder will be associated with the workout when it is finished.
--
-- @seriesType@ — The series type for which the builder should be retrieved.
--
-- ObjC selector: @- seriesBuilderForType:@
seriesBuilderForType :: (IsHKWorkoutBuilder hkWorkoutBuilder, IsHKSeriesType seriesType) => hkWorkoutBuilder -> seriesType -> IO (Id HKSeriesBuilder)
seriesBuilderForType hkWorkoutBuilder  seriesType =
  withObjCPtr seriesType $ \raw_seriesType ->
      sendMsg hkWorkoutBuilder (mkSelector "seriesBuilderForType:") (retPtr retVoid) [argPtr (castPtr raw_seriesType :: Ptr ())] >>= retainedObject . castPtr

-- | device
--
-- The HKDevice to be associated with the workout.
--
-- ObjC selector: @- device@
device :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id HKDevice)
device hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | startDate
--
-- The start date for the workout, as provided by beginCollectionWithStartDate:completion:
--
-- ObjC selector: @- startDate@
startDate :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSDate)
startDate hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | endDate
--
-- The end date for the workout, as provided by endCollectionWithEndDate:completion:
--
-- ObjC selector: @- endDate@
endDate :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSDate)
endDate hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | workoutConfiguration
--
-- The configuration for the workout being built.
--
-- ObjC selector: @- workoutConfiguration@
workoutConfiguration :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id HKWorkoutConfiguration)
workoutConfiguration hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "workoutConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- The metadata that will be used when the workout is finished.
--
-- ObjC selector: @- metadata@
metadata :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSDictionary)
metadata hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | workoutEvents
--
-- Workout events that have been added to the builder.
--
-- New events that are added using addWorkoutEvents:completion: will be appended to this array once the                completion is called.
--
-- ObjC selector: @- workoutEvents@
workoutEvents :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSArray)
workoutEvents hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "workoutEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | workoutActivities
--
-- Workout activities that have been added to the builder.
--
-- New activities that are added using addWorkoutActivity:completion: will be appended to this array once the                completion is called.
--
-- ObjC selector: @- workoutActivities@
workoutActivities :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSArray)
workoutActivities hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "workoutActivities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | allStatistics
--
-- A dictionary of statistics per quantity type added to the builder
--
-- This dictionary will contain HKStatistics objects containing the statistics by quantity                sample type for all of the samples that have been added to the builder.
--
-- ObjC selector: @- allStatistics@
allStatistics :: IsHKWorkoutBuilder hkWorkoutBuilder => hkWorkoutBuilder -> IO (Id NSDictionary)
allStatistics hkWorkoutBuilder  =
    sendMsg hkWorkoutBuilder (mkSelector "allStatistics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithHealthStore:configuration:device:@
initWithHealthStore_configuration_deviceSelector :: Selector
initWithHealthStore_configuration_deviceSelector = mkSelector "initWithHealthStore:configuration:device:"

-- | @Selector@ for @beginCollectionWithStartDate:completion:@
beginCollectionWithStartDate_completionSelector :: Selector
beginCollectionWithStartDate_completionSelector = mkSelector "beginCollectionWithStartDate:completion:"

-- | @Selector@ for @addSamples:completion:@
addSamples_completionSelector :: Selector
addSamples_completionSelector = mkSelector "addSamples:completion:"

-- | @Selector@ for @addWorkoutEvents:completion:@
addWorkoutEvents_completionSelector :: Selector
addWorkoutEvents_completionSelector = mkSelector "addWorkoutEvents:completion:"

-- | @Selector@ for @addMetadata:completion:@
addMetadata_completionSelector :: Selector
addMetadata_completionSelector = mkSelector "addMetadata:completion:"

-- | @Selector@ for @addWorkoutActivity:completion:@
addWorkoutActivity_completionSelector :: Selector
addWorkoutActivity_completionSelector = mkSelector "addWorkoutActivity:completion:"

-- | @Selector@ for @updateActivityWithUUID:endDate:completion:@
updateActivityWithUUID_endDate_completionSelector :: Selector
updateActivityWithUUID_endDate_completionSelector = mkSelector "updateActivityWithUUID:endDate:completion:"

-- | @Selector@ for @updateActivityWithUUID:addMedatata:completion:@
updateActivityWithUUID_addMedatata_completionSelector :: Selector
updateActivityWithUUID_addMedatata_completionSelector = mkSelector "updateActivityWithUUID:addMedatata:completion:"

-- | @Selector@ for @endCollectionWithEndDate:completion:@
endCollectionWithEndDate_completionSelector :: Selector
endCollectionWithEndDate_completionSelector = mkSelector "endCollectionWithEndDate:completion:"

-- | @Selector@ for @finishWorkoutWithCompletion:@
finishWorkoutWithCompletionSelector :: Selector
finishWorkoutWithCompletionSelector = mkSelector "finishWorkoutWithCompletion:"

-- | @Selector@ for @discardWorkout@
discardWorkoutSelector :: Selector
discardWorkoutSelector = mkSelector "discardWorkout"

-- | @Selector@ for @elapsedTimeAtDate:@
elapsedTimeAtDateSelector :: Selector
elapsedTimeAtDateSelector = mkSelector "elapsedTimeAtDate:"

-- | @Selector@ for @statisticsForType:@
statisticsForTypeSelector :: Selector
statisticsForTypeSelector = mkSelector "statisticsForType:"

-- | @Selector@ for @seriesBuilderForType:@
seriesBuilderForTypeSelector :: Selector
seriesBuilderForTypeSelector = mkSelector "seriesBuilderForType:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @workoutConfiguration@
workoutConfigurationSelector :: Selector
workoutConfigurationSelector = mkSelector "workoutConfiguration"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @workoutEvents@
workoutEventsSelector :: Selector
workoutEventsSelector = mkSelector "workoutEvents"

-- | @Selector@ for @workoutActivities@
workoutActivitiesSelector :: Selector
workoutActivitiesSelector = mkSelector "workoutActivities"

-- | @Selector@ for @allStatistics@
allStatisticsSelector :: Selector
allStatisticsSelector = mkSelector "allStatistics"

