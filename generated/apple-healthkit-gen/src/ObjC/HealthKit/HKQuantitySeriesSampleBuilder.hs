{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantitySeriesSampleBuilder
--
-- An HKQuantitySeriesSampleBuilder is used to generate HKQuantitySample(s) with multiple                quantities.
--
-- An HKQuantitySeriesSampleBuilder is used to incrementally create a new quantity series                sample in the HealthKit database. This class may be used to create long-running quantity                series samples that are associated with an activity like a workout. After inserting each                of the quantities that make up the series, the series may be finalized by calling                -finishSeriesWithMetadata:completion:. Calling -discard invalidates the series and                discards any data that was previously associated with it.
--
-- Generated bindings for @HKQuantitySeriesSampleBuilder@.
module ObjC.HealthKit.HKQuantitySeriesSampleBuilder
  ( HKQuantitySeriesSampleBuilder
  , IsHKQuantitySeriesSampleBuilder(..)
  , initWithHealthStore_quantityType_startDate_device
  , init_
  , insertQuantity_dateInterval_error
  , insertQuantity_date_error
  , discard
  , quantityType
  , startDate
  , device
  , initWithHealthStore_quantityType_startDate_deviceSelector
  , initSelector
  , insertQuantity_dateInterval_errorSelector
  , insertQuantity_date_errorSelector
  , discardSelector
  , quantityTypeSelector
  , startDateSelector
  , deviceSelector


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

-- | initWithHealthStore:quantityType:device:
--
-- The designated initializer to create an HKQuantitySeriesSampleBuilder.
--
-- The HKHealthStore is retained during the life of the object for the saving of the                series data and final return of the series sample.
--
-- @healthStore@ — Specifies the HKHealthStore object to use for building the series.
--
-- @quantityType@ — Specifies the quantity type for which to build the series.
--
-- @startDate@ — The date from which the produced sample(s) start.
--
-- @device@ — The optional device represents the HKDevice from which the data is                                provided.
--
-- ObjC selector: @- initWithHealthStore:quantityType:startDate:device:@
initWithHealthStore_quantityType_startDate_device :: (IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder, IsHKHealthStore healthStore, IsHKQuantityType quantityType, IsNSDate startDate, IsHKDevice device) => hkQuantitySeriesSampleBuilder -> healthStore -> quantityType -> startDate -> device -> IO (Id HKQuantitySeriesSampleBuilder)
initWithHealthStore_quantityType_startDate_device hkQuantitySeriesSampleBuilder  healthStore quantityType startDate device =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr device $ \raw_device ->
            sendMsg hkQuantitySeriesSampleBuilder (mkSelector "initWithHealthStore:quantityType:startDate:device:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder => hkQuantitySeriesSampleBuilder -> IO (Id HKQuantitySeriesSampleBuilder)
init_ hkQuantitySeriesSampleBuilder  =
    sendMsg hkQuantitySeriesSampleBuilder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | insertQuantity:dateInterval:completion:
--
-- Associate a new quantity with the receiver with a specific date interval.
--
-- Use this method to add a quantity to the series. The quantity must have a unit                    that is compatible with the receiver's quantity type.                    See -[HKQuantityType isCompatibleWithUnit:].                    Note that quantities may be inserted in any order,                    but will be sorted by dateInterval.startDate when the series is finished.
--
-- @quantity@ — The quantity to insert.
--
-- @dateInterval@ — The dateInterval associated with the quantity.                                    If dateInterval.startDate is the same as a previously-provided                                    quantity, the new value will replace the old value.                                    An HKErrorInvalidArgument will be returned if                                    dateInterval.startDate is earlier than the receiver's startDate.
--
-- ObjC selector: @- insertQuantity:dateInterval:error:@
insertQuantity_dateInterval_error :: (IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder, IsHKQuantity quantity, IsNSDateInterval dateInterval, IsNSError error_) => hkQuantitySeriesSampleBuilder -> quantity -> dateInterval -> error_ -> IO Bool
insertQuantity_dateInterval_error hkQuantitySeriesSampleBuilder  quantity dateInterval error_ =
  withObjCPtr quantity $ \raw_quantity ->
    withObjCPtr dateInterval $ \raw_dateInterval ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantitySeriesSampleBuilder (mkSelector "insertQuantity:dateInterval:error:") retCULong [argPtr (castPtr raw_quantity :: Ptr ()), argPtr (castPtr raw_dateInterval :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | insertQuantity:date:completion:
--
-- Associate a new quantity with the receiver at a specific instantaneous                    date interval.
--
-- This method acts as a convenience for insertQuantity:dateInterval:completion:                    where dateInterval has a duration of 0.
--
-- @quantity@ — The quantity to insert.
--
-- @date@ — The start date associated with the quantity. If this is the same                                start date as a previously-provided quantity, the new value will                                replace the old value. An HKErrorInvalidArgument will be returned                                if date is earlier than the receiver's startDate.
--
-- ObjC selector: @- insertQuantity:date:error:@
insertQuantity_date_error :: (IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder, IsHKQuantity quantity, IsNSDate date, IsNSError error_) => hkQuantitySeriesSampleBuilder -> quantity -> date -> error_ -> IO Bool
insertQuantity_date_error hkQuantitySeriesSampleBuilder  quantity date error_ =
  withObjCPtr quantity $ \raw_quantity ->
    withObjCPtr date $ \raw_date ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantitySeriesSampleBuilder (mkSelector "insertQuantity:date:error:") retCULong [argPtr (castPtr raw_quantity :: Ptr ()), argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | discard
--
-- Discards all previously inserted data and invalidates the series.
--
-- Calling this method will delete all quantities that were previously inserted into                    the series and invalidate the receiver. Calling other methods on the receiver                    after calling -discard will result in an exception.
--
-- ObjC selector: @- discard@
discard :: IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder => hkQuantitySeriesSampleBuilder -> IO ()
discard hkQuantitySeriesSampleBuilder  =
    sendMsg hkQuantitySeriesSampleBuilder (mkSelector "discard") retVoid []

-- | quantityType
--
-- ObjC selector: @- quantityType@
quantityType :: IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder => hkQuantitySeriesSampleBuilder -> IO (Id HKQuantityType)
quantityType hkQuantitySeriesSampleBuilder  =
    sendMsg hkQuantitySeriesSampleBuilder (mkSelector "quantityType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | startDate
--
-- ObjC selector: @- startDate@
startDate :: IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder => hkQuantitySeriesSampleBuilder -> IO (Id NSDate)
startDate hkQuantitySeriesSampleBuilder  =
    sendMsg hkQuantitySeriesSampleBuilder (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | device
--
-- ObjC selector: @- device@
device :: IsHKQuantitySeriesSampleBuilder hkQuantitySeriesSampleBuilder => hkQuantitySeriesSampleBuilder -> IO (Id HKDevice)
device hkQuantitySeriesSampleBuilder  =
    sendMsg hkQuantitySeriesSampleBuilder (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHealthStore:quantityType:startDate:device:@
initWithHealthStore_quantityType_startDate_deviceSelector :: Selector
initWithHealthStore_quantityType_startDate_deviceSelector = mkSelector "initWithHealthStore:quantityType:startDate:device:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @insertQuantity:dateInterval:error:@
insertQuantity_dateInterval_errorSelector :: Selector
insertQuantity_dateInterval_errorSelector = mkSelector "insertQuantity:dateInterval:error:"

-- | @Selector@ for @insertQuantity:date:error:@
insertQuantity_date_errorSelector :: Selector
insertQuantity_date_errorSelector = mkSelector "insertQuantity:date:error:"

-- | @Selector@ for @discard@
discardSelector :: Selector
discardSelector = mkSelector "discard"

-- | @Selector@ for @quantityType@
quantityTypeSelector :: Selector
quantityTypeSelector = mkSelector "quantityType"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

