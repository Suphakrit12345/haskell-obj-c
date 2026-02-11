{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKQuantitySeriesSampleQuery@.
module ObjC.HealthKit.HKQuantitySeriesSampleQuery
  ( HKQuantitySeriesSampleQuery
  , IsHKQuantitySeriesSampleQuery(..)
  , initWithQuantityType_predicate_quantityHandler
  , initWithSample_quantityHandler
  , includeSample
  , setIncludeSample
  , orderByQuantitySampleStartDate
  , setOrderByQuantitySampleStartDate
  , initWithQuantityType_predicate_quantityHandlerSelector
  , initWithSample_quantityHandlerSelector
  , includeSampleSelector
  , setIncludeSampleSelector
  , orderByQuantitySampleStartDateSelector
  , setOrderByQuantitySampleStartDateSelector


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

-- | initWithSample:dataHandler:
--
-- Returns a query that will retrieve HKQuantity objects for samples of a specified                type that match the specified predicate.
--
-- @quantityType@ — The type of HKQuantitySample to retrieve.
--
-- @predicate@ — The predicate which the query results should match.                                    To query for the quantities for a specific quantity sample                                    see: +[HKPredicates predicateForObjectWithUUID:]
--
-- @quantityHandler@ — The block to invoke with results from the query. It will be                                    called repeatedly with HKQuantity, and NSDateInterval objects in                                    ascending dateInterval.startDate order, until all quantities are                                    returned and the done parameter is YES                                    or -[HKHealthStore stopQuery:] is called.                                    The quantitySample parameter is nil unless includeSample is YES,                                    in which case it will be the quantitySample which owns the current                                    quantity anytime the quantity paramater is non-nil.                                    The stopQuery call can be made within the quantityHandler block.                                    Once done is YES, or stopQuery has been called, the query is                                    complete and no more calls to quantityHandler will be made.
--
-- ObjC selector: @- initWithQuantityType:predicate:quantityHandler:@
initWithQuantityType_predicate_quantityHandler :: (IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery, IsHKQuantityType quantityType, IsNSPredicate predicate) => hkQuantitySeriesSampleQuery -> quantityType -> predicate -> Ptr () -> IO (Id HKQuantitySeriesSampleQuery)
initWithQuantityType_predicate_quantityHandler hkQuantitySeriesSampleQuery  quantityType predicate quantityHandler =
  withObjCPtr quantityType $ \raw_quantityType ->
    withObjCPtr predicate $ \raw_predicate ->
        sendMsg hkQuantitySeriesSampleQuery (mkSelector "initWithQuantityType:predicate:quantityHandler:") (retPtr retVoid) [argPtr (castPtr raw_quantityType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr quantityHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSample:quantityHandler:@
initWithSample_quantityHandler :: (IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery, IsHKQuantitySample quantitySample) => hkQuantitySeriesSampleQuery -> quantitySample -> Ptr () -> IO (Id HKQuantitySeriesSampleQuery)
initWithSample_quantityHandler hkQuantitySeriesSampleQuery  quantitySample quantityHandler =
  withObjCPtr quantitySample $ \raw_quantitySample ->
      sendMsg hkQuantitySeriesSampleQuery (mkSelector "initWithSample:quantityHandler:") (retPtr retVoid) [argPtr (castPtr raw_quantitySample :: Ptr ()), argPtr (castPtr quantityHandler :: Ptr ())] >>= ownedObject . castPtr

-- | includeSample
--
-- Include owning HKQuantitySample in quantityHandler handler.
--
-- Default value is NO.                If includeSample is set then the quantitySample parameter of quantityHandler will                be non-nil anytime the quantity parameter is non-nil.                Specifying this option has a performance cost.                This property may not be modified once the query has been executed.
--
-- ObjC selector: @- includeSample@
includeSample :: IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery => hkQuantitySeriesSampleQuery -> IO Bool
includeSample hkQuantitySeriesSampleQuery  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantitySeriesSampleQuery (mkSelector "includeSample") retCULong []

-- | includeSample
--
-- Include owning HKQuantitySample in quantityHandler handler.
--
-- Default value is NO.                If includeSample is set then the quantitySample parameter of quantityHandler will                be non-nil anytime the quantity parameter is non-nil.                Specifying this option has a performance cost.                This property may not be modified once the query has been executed.
--
-- ObjC selector: @- setIncludeSample:@
setIncludeSample :: IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery => hkQuantitySeriesSampleQuery -> Bool -> IO ()
setIncludeSample hkQuantitySeriesSampleQuery  value =
    sendMsg hkQuantitySeriesSampleQuery (mkSelector "setIncludeSample:") retVoid [argCULong (if value then 1 else 0)]

-- | orderByQuantitySampleStartDate
--
-- Order enumerated results first by quantitySample.startDate,                then by the quantity's dateInterval.startDate.
--
-- Default value is NO.                All quantities owned by a given quantitySample will be                enumerated before any quantities owned by any other quantity sample,                and the quantity samples will be enumerated in their startDate order.                Note that individual quantities may not be returned in their                dateInterval.startDate order if more than one quantitySample overlap in time.                This property may not be modified once the query has been executed.
--
-- ObjC selector: @- orderByQuantitySampleStartDate@
orderByQuantitySampleStartDate :: IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery => hkQuantitySeriesSampleQuery -> IO Bool
orderByQuantitySampleStartDate hkQuantitySeriesSampleQuery  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantitySeriesSampleQuery (mkSelector "orderByQuantitySampleStartDate") retCULong []

-- | orderByQuantitySampleStartDate
--
-- Order enumerated results first by quantitySample.startDate,                then by the quantity's dateInterval.startDate.
--
-- Default value is NO.                All quantities owned by a given quantitySample will be                enumerated before any quantities owned by any other quantity sample,                and the quantity samples will be enumerated in their startDate order.                Note that individual quantities may not be returned in their                dateInterval.startDate order if more than one quantitySample overlap in time.                This property may not be modified once the query has been executed.
--
-- ObjC selector: @- setOrderByQuantitySampleStartDate:@
setOrderByQuantitySampleStartDate :: IsHKQuantitySeriesSampleQuery hkQuantitySeriesSampleQuery => hkQuantitySeriesSampleQuery -> Bool -> IO ()
setOrderByQuantitySampleStartDate hkQuantitySeriesSampleQuery  value =
    sendMsg hkQuantitySeriesSampleQuery (mkSelector "setOrderByQuantitySampleStartDate:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuantityType:predicate:quantityHandler:@
initWithQuantityType_predicate_quantityHandlerSelector :: Selector
initWithQuantityType_predicate_quantityHandlerSelector = mkSelector "initWithQuantityType:predicate:quantityHandler:"

-- | @Selector@ for @initWithSample:quantityHandler:@
initWithSample_quantityHandlerSelector :: Selector
initWithSample_quantityHandlerSelector = mkSelector "initWithSample:quantityHandler:"

-- | @Selector@ for @includeSample@
includeSampleSelector :: Selector
includeSampleSelector = mkSelector "includeSample"

-- | @Selector@ for @setIncludeSample:@
setIncludeSampleSelector :: Selector
setIncludeSampleSelector = mkSelector "setIncludeSample:"

-- | @Selector@ for @orderByQuantitySampleStartDate@
orderByQuantitySampleStartDateSelector :: Selector
orderByQuantitySampleStartDateSelector = mkSelector "orderByQuantitySampleStartDate"

-- | @Selector@ for @setOrderByQuantitySampleStartDate:@
setOrderByQuantitySampleStartDateSelector :: Selector
setOrderByQuantitySampleStartDateSelector = mkSelector "setOrderByQuantitySampleStartDate:"

