{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKLiveWorkoutDataSource
--
-- An HKLiveWorkoutDataSource is to be used with an HKWorkoutBuilder to automatically collect samples
--
-- Generated bindings for @HKLiveWorkoutDataSource@.
module ObjC.HealthKit.HKLiveWorkoutDataSource
  ( HKLiveWorkoutDataSource
  , IsHKLiveWorkoutDataSource(..)
  , init_
  , initWithHealthStore_workoutConfiguration
  , enableCollectionForType_predicate
  , disableCollectionForType
  , typesToCollect
  , initSelector
  , initWithHealthStore_workoutConfigurationSelector
  , enableCollectionForType_predicateSelector
  , disableCollectionForTypeSelector
  , typesToCollectSelector


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
init_ :: IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource => hkLiveWorkoutDataSource -> IO (Id HKLiveWorkoutDataSource)
init_ hkLiveWorkoutDataSource  =
    sendMsg hkLiveWorkoutDataSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithHealthStore:workoutConfiguration:
--
-- The designated initializer of HKLiveWorkoutDataSource.
--
-- @healthStore@ — The HKHealthStore. This should match the one used to create the corresponding                                HKWorkoutBuilder.
--
-- @configuration@ — An optional workout configuration. typesToCollect will be populated with default                                types for the workout configuration
--
-- ObjC selector: @- initWithHealthStore:workoutConfiguration:@
initWithHealthStore_workoutConfiguration :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKHealthStore healthStore, IsHKWorkoutConfiguration configuration) => hkLiveWorkoutDataSource -> healthStore -> configuration -> IO (Id HKLiveWorkoutDataSource)
initWithHealthStore_workoutConfiguration hkLiveWorkoutDataSource  healthStore configuration =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg hkLiveWorkoutDataSource (mkSelector "initWithHealthStore:workoutConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | enableCollectionForType:predicate
--
-- Adds a new type of quantity sample to collect.
--
-- Calling this method for a type that is already being collected will override the predicate for that type.
--
-- @quantityType@ — The type of sample to collect.
--
-- @predicate@ — If non-nil, collected samples must match this predicate.
--
-- ObjC selector: @- enableCollectionForType:predicate:@
enableCollectionForType_predicate :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKQuantityType quantityType, IsNSPredicate predicate) => hkLiveWorkoutDataSource -> quantityType -> predicate -> IO ()
enableCollectionForType_predicate hkLiveWorkoutDataSource  quantityType predicate =
  withObjCPtr quantityType $ \raw_quantityType ->
    withObjCPtr predicate $ \raw_predicate ->
        sendMsg hkLiveWorkoutDataSource (mkSelector "enableCollectionForType:predicate:") retVoid [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ())]

-- | disableCollectionForType:
--
-- Removes the specified quantity type from the types to collect.
--
-- @quantityType@ — The type of sample to no longer collect.
--
-- ObjC selector: @- disableCollectionForType:@
disableCollectionForType :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKQuantityType quantityType) => hkLiveWorkoutDataSource -> quantityType -> IO ()
disableCollectionForType hkLiveWorkoutDataSource  quantityType =
  withObjCPtr quantityType $ \raw_quantityType ->
      sendMsg hkLiveWorkoutDataSource (mkSelector "disableCollectionForType:") retVoid [argPtr (castPtr raw_quantityType :: Ptr ())]

-- | typesToCollect
--
-- The quantity types the receiver is collecting.
--
-- ObjC selector: @- typesToCollect@
typesToCollect :: IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource => hkLiveWorkoutDataSource -> IO (Id NSSet)
typesToCollect hkLiveWorkoutDataSource  =
    sendMsg hkLiveWorkoutDataSource (mkSelector "typesToCollect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithHealthStore:workoutConfiguration:@
initWithHealthStore_workoutConfigurationSelector :: Selector
initWithHealthStore_workoutConfigurationSelector = mkSelector "initWithHealthStore:workoutConfiguration:"

-- | @Selector@ for @enableCollectionForType:predicate:@
enableCollectionForType_predicateSelector :: Selector
enableCollectionForType_predicateSelector = mkSelector "enableCollectionForType:predicate:"

-- | @Selector@ for @disableCollectionForType:@
disableCollectionForTypeSelector :: Selector
disableCollectionForTypeSelector = mkSelector "disableCollectionForType:"

-- | @Selector@ for @typesToCollect@
typesToCollectSelector :: Selector
typesToCollectSelector = mkSelector "typesToCollect"

