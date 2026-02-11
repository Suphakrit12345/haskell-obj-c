{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantitySample
--
-- An abstract HKSample subclass representing a quantity measurement.
--
-- Generated bindings for @HKQuantitySample@.
module ObjC.HealthKit.HKQuantitySample
  ( HKQuantitySample
  , IsHKQuantitySample(..)
  , quantitySampleWithType_quantity_startDate_endDate
  , quantitySampleWithType_quantity_startDate_endDate_metadata
  , quantitySampleWithType_quantity_startDate_endDate_device_metadata
  , quantityType
  , quantity
  , count
  , quantitySampleWithType_quantity_startDate_endDateSelector
  , quantitySampleWithType_quantity_startDate_endDate_metadataSelector
  , quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector
  , quantityTypeSelector
  , quantitySelector
  , countSelector


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

-- | quantitySampleWithType:quantity:startDate:endDate:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, and end date.
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:@
quantitySampleWithType_quantity_startDate_endDate :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate) => quantityType -> quantity -> startDate -> endDate -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate quantityType quantity startDate endDate =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr quantity $ \raw_quantity ->
        withObjCPtr startDate $ \raw_startDate ->
          withObjCPtr endDate $ \raw_endDate ->
            sendClassMsg cls' (mkSelector "quantitySampleWithType:quantity:startDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_quantity :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= retainedObject . castPtr

-- | quantitySampleWithType:quantity:startDate:endDate:metadata:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, end date, and metadata.
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:metadata:@
quantitySampleWithType_quantity_startDate_endDate_metadata :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => quantityType -> quantity -> startDate -> endDate -> metadata -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_metadata quantityType quantity startDate endDate metadata =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr quantity $ \raw_quantity ->
        withObjCPtr startDate $ \raw_startDate ->
          withObjCPtr endDate $ \raw_endDate ->
            withObjCPtr metadata $ \raw_metadata ->
              sendClassMsg cls' (mkSelector "quantitySampleWithType:quantity:startDate:endDate:metadata:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_quantity :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | quantitySampleWithType:quantity:startDate:endDate:device:metadata:
--
-- Creates a new HKQuantitySample with the given type, quantity, start date, end date, and metadata.
--
-- @quantityType@ — The type of the sample.
--
-- @startDate@ — The start date of the sample.
--
-- @endDate@ — The end date of the sample.
--
-- @device@ — The HKDevice that generated the sample (optional).
--
-- @metadata@ — Metadata for the sample (optional).
--
-- The quantity must have a unit that is compatible with the given quantity type.                See -[HKQuantityType isCompatibleWithUnit:].
--
-- ObjC selector: @+ quantitySampleWithType:quantity:startDate:endDate:device:metadata:@
quantitySampleWithType_quantity_startDate_endDate_device_metadata :: (IsHKQuantityType quantityType, IsHKQuantity quantity, IsNSDate startDate, IsNSDate endDate, IsHKDevice device, IsNSDictionary metadata) => quantityType -> quantity -> startDate -> endDate -> device -> metadata -> IO (Id HKQuantitySample)
quantitySampleWithType_quantity_startDate_endDate_device_metadata quantityType quantity startDate endDate device metadata =
  do
    cls' <- getRequiredClass "HKQuantitySample"
    withObjCPtr quantityType $ \raw_quantityType ->
      withObjCPtr quantity $ \raw_quantity ->
        withObjCPtr startDate $ \raw_startDate ->
          withObjCPtr endDate $ \raw_endDate ->
            withObjCPtr device $ \raw_device ->
              withObjCPtr metadata $ \raw_metadata ->
                sendClassMsg cls' (mkSelector "quantitySampleWithType:quantity:startDate:endDate:device:metadata:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_quantity :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- quantityType@
quantityType :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO (Id HKQuantityType)
quantityType hkQuantitySample  =
    sendMsg hkQuantitySample (mkSelector "quantityType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quantity@
quantity :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO (Id HKQuantity)
quantity hkQuantitySample  =
    sendMsg hkQuantitySample (mkSelector "quantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | count
--
-- The number of individual values making up the receiver's quantity.
--
-- Requests for the individual series quantities can be made using HKQuantitySeriesSampleQuery.
--
-- ObjC selector: @- count@
count :: IsHKQuantitySample hkQuantitySample => hkQuantitySample -> IO CLong
count hkQuantitySample  =
    sendMsg hkQuantitySample (mkSelector "count") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:@
quantitySampleWithType_quantity_startDate_endDateSelector :: Selector
quantitySampleWithType_quantity_startDate_endDateSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:"

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:metadata:@
quantitySampleWithType_quantity_startDate_endDate_metadataSelector :: Selector
quantitySampleWithType_quantity_startDate_endDate_metadataSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:metadata:"

-- | @Selector@ for @quantitySampleWithType:quantity:startDate:endDate:device:metadata:@
quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector :: Selector
quantitySampleWithType_quantity_startDate_endDate_device_metadataSelector = mkSelector "quantitySampleWithType:quantity:startDate:endDate:device:metadata:"

-- | @Selector@ for @quantityType@
quantityTypeSelector :: Selector
quantityTypeSelector = mkSelector "quantityType"

-- | @Selector@ for @quantity@
quantitySelector :: Selector
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

