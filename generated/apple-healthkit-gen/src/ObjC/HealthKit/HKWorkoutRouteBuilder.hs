{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutRouteBuilder
--
-- An HKWorkoutRouteBuilder is used to generate an HKWorkoutRoute.
--
-- This class is intended for generating long-running location data collection such as                     might be associated with a workout. If the discard method is called, collected data will be deleted.                     Calling finishRouteWithWorkout:metadata: will stop and complete the route. If the builder is deleted,                     or the client goes away before calling the finish method, data will be lost.
--
-- Generated bindings for @HKWorkoutRouteBuilder@.
module ObjC.HealthKit.HKWorkoutRouteBuilder
  ( HKWorkoutRouteBuilder
  , IsHKWorkoutRouteBuilder(..)
  , initWithHealthStore_device
  , insertRouteData_completion
  , addMetadata_completion
  , finishRouteWithWorkout_metadata_completion
  , initWithHealthStore_deviceSelector
  , insertRouteData_completionSelector
  , addMetadata_completionSelector
  , finishRouteWithWorkout_metadata_completionSelector


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

-- | initWithHealthStore:device:
--
-- The designated initializer to create an HKWorkoutRouteBuilder. If you are using an HKWorkoutBuilder , you                     should not create an HKWorkoutRouteBuilder, instead use -[HKWorkoutBuilder seriesBuilderForType:]
--
-- The HKHealthStore is retained during the life of the object for the saving of the series data and final                     return of the series sample.
--
-- @healthStore@ — Specifies the HKHealthStore object to use for building the series.
--
-- @device@ — The optional device represents the HKDevice from which the data is provided.
--
-- ObjC selector: @- initWithHealthStore:device:@
initWithHealthStore_device :: (IsHKWorkoutRouteBuilder hkWorkoutRouteBuilder, IsHKHealthStore healthStore, IsHKDevice device) => hkWorkoutRouteBuilder -> healthStore -> device -> IO (Id HKWorkoutRouteBuilder)
initWithHealthStore_device hkWorkoutRouteBuilder  healthStore device =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr device $ \raw_device ->
        sendMsg hkWorkoutRouteBuilder (mkSelector "initWithHealthStore:device:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | insertRouteData:completion:
--
-- Associate CLLocation with the receiver.
--
-- Use this method to asynchronously add one or more CLLocation to the                     series. Note that CLLocation may be inserted in any order but will be                     sorted according to date when the series is finalized.
--
-- @routeData@ — An array of one or more CLLocation.
--
-- @completion@ — The completion callback handler returns the status of the save. If the completion handler success is                     NO, then error is non-nil. An error here is considered fatal and the series builder will be complete.                     If data was previously saved, then the HKWorkoutRoute may be retrieved by the                     finishRouteWithMetadata: method.
--
-- ObjC selector: @- insertRouteData:completion:@
insertRouteData_completion :: (IsHKWorkoutRouteBuilder hkWorkoutRouteBuilder, IsNSArray routeData) => hkWorkoutRouteBuilder -> routeData -> Ptr () -> IO ()
insertRouteData_completion hkWorkoutRouteBuilder  routeData completion =
  withObjCPtr routeData $ \raw_routeData ->
      sendMsg hkWorkoutRouteBuilder (mkSelector "insertRouteData:completion:") retVoid [argPtr (castPtr raw_routeData :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | addMetadata:completion:
--
-- Adds new metadata to the builder instance. This method can be called more than once; each time                     the newly provided metadata will be incorporated in the same manner as                     -[NSMutableDictionary addEntriesFromDictionary:].                     This operation is performed asynchronously and the completion will be executed on an arbitrary                     background queue.
--
-- @metadata@ — The metadata to add to the builder.
--
-- @completion@ — Block to be called when the addition of metadata to the builder is complete. If success is YES, the                     metadata has been added to the builder successfully. If success is NO, error will be non-null and                     will contain the error encountered during the insertion operation. When an error occurs, the builder's                     metadata will remain unchanged.
--
-- ObjC selector: @- addMetadata:completion:@
addMetadata_completion :: (IsHKWorkoutRouteBuilder hkWorkoutRouteBuilder, IsNSDictionary metadata) => hkWorkoutRouteBuilder -> metadata -> Ptr () -> IO ()
addMetadata_completion hkWorkoutRouteBuilder  metadata completion =
  withObjCPtr metadata $ \raw_metadata ->
      sendMsg hkWorkoutRouteBuilder (mkSelector "addMetadata:completion:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | finishRouteWithWorkout:Metadata:completion:
--
-- Method to stop data collection and return the associated HKWorkoutRoute. If you are using this route                     builder with a workout builder, you should never call this method. The route will be finished when you                     finish the workout builder.
--
-- Call this method when the route has been completed. The completion handler will return the saved HKWorkoutRoute.     If no series data was added, then workoutRoute will be nil and an error returned. The                     receiver will be considered invalid afterwards and any further calls to it will result in an error.
--
-- @workout@ — The HKWorkout object to which the route will be associated. Must be saved to HealthKit
--
-- @metadata@ — Optional metadata may be added to associate with the series. Predefined keys are found in                     HKMetadata.h, or private NSString keys used by the client are allowed. Acceptable metadata value                     types are NSString, NSDate, NSNumber and HKQuantity
--
-- @completion@ — The completion callback handler returns the saved HKWorkoutRoute object. If workoutRoute is nil, an                     error will indicate why the series could not be returned including database inaccessibility during                     device lock. Subsequent requests for the HKWorkoutRoute can be made through HKSampleQuery or similar                     queries. workoutRoute cannot be associated to another workout.
--
-- ObjC selector: @- finishRouteWithWorkout:metadata:completion:@
finishRouteWithWorkout_metadata_completion :: (IsHKWorkoutRouteBuilder hkWorkoutRouteBuilder, IsHKWorkout workout, IsNSDictionary metadata) => hkWorkoutRouteBuilder -> workout -> metadata -> Ptr () -> IO ()
finishRouteWithWorkout_metadata_completion hkWorkoutRouteBuilder  workout metadata completion =
  withObjCPtr workout $ \raw_workout ->
    withObjCPtr metadata $ \raw_metadata ->
        sendMsg hkWorkoutRouteBuilder (mkSelector "finishRouteWithWorkout:metadata:completion:") retVoid [argPtr (castPtr raw_workout :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHealthStore:device:@
initWithHealthStore_deviceSelector :: Selector
initWithHealthStore_deviceSelector = mkSelector "initWithHealthStore:device:"

-- | @Selector@ for @insertRouteData:completion:@
insertRouteData_completionSelector :: Selector
insertRouteData_completionSelector = mkSelector "insertRouteData:completion:"

-- | @Selector@ for @addMetadata:completion:@
addMetadata_completionSelector :: Selector
addMetadata_completionSelector = mkSelector "addMetadata:completion:"

-- | @Selector@ for @finishRouteWithWorkout:metadata:completion:@
finishRouteWithWorkout_metadata_completionSelector :: Selector
finishRouteWithWorkout_metadata_completionSelector = mkSelector "finishRouteWithWorkout:metadata:completion:"

