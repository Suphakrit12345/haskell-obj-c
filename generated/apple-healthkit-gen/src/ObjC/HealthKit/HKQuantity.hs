{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantity
--
-- The HKQuantity class provides an encapsulation of a quantity value and the unit of measurement.
--
-- Generated bindings for @HKQuantity@.
module ObjC.HealthKit.HKQuantity
  ( HKQuantity
  , IsHKQuantity(..)
  , init_
  , quantityWithUnit_doubleValue
  , isCompatibleWithUnit
  , doubleValueForUnit
  , compare_
  , initSelector
  , quantityWithUnit_doubleValueSelector
  , isCompatibleWithUnitSelector
  , doubleValueForUnitSelector
  , compareSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKQuantity hkQuantity => hkQuantity -> IO (Id HKQuantity)
init_ hkQuantity  =
    sendMsg hkQuantity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | quantityWithUnit:doubleValue:
--
-- Returns a new object representing a quantity measurement with the given unit.
--
-- ObjC selector: @+ quantityWithUnit:doubleValue:@
quantityWithUnit_doubleValue :: IsHKUnit unit => unit -> CDouble -> IO (Id HKQuantity)
quantityWithUnit_doubleValue unit value =
  do
    cls' <- getRequiredClass "HKQuantity"
    withObjCPtr unit $ \raw_unit ->
      sendClassMsg cls' (mkSelector "quantityWithUnit:doubleValue:") (retPtr retVoid) [argPtr (castPtr raw_unit :: Ptr ()), argCDouble value] >>= retainedObject . castPtr

-- | isCompatibleWithUnit:
--
-- Returns yes if the receiver's value can be converted to a value of the given unit.
--
-- ObjC selector: @- isCompatibleWithUnit:@
isCompatibleWithUnit :: (IsHKQuantity hkQuantity, IsHKUnit unit) => hkQuantity -> unit -> IO Bool
isCompatibleWithUnit hkQuantity  unit =
  withObjCPtr unit $ \raw_unit ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantity (mkSelector "isCompatibleWithUnit:") retCULong [argPtr (castPtr raw_unit :: Ptr ())]

-- | doubleValueForUnit:
--
-- Returns the quantity value converted to the given unit.
--
-- Throws an exception if the receiver's value cannot be converted to one of the requested unit.
--
-- ObjC selector: @- doubleValueForUnit:@
doubleValueForUnit :: (IsHKQuantity hkQuantity, IsHKUnit unit) => hkQuantity -> unit -> IO CDouble
doubleValueForUnit hkQuantity  unit =
  withObjCPtr unit $ \raw_unit ->
      sendMsg hkQuantity (mkSelector "doubleValueForUnit:") retCDouble [argPtr (castPtr raw_unit :: Ptr ())]

-- | compare:
--
-- Returns an NSComparisonResult value that indicates whether the receiver is greater than, equal to, or                 less than a given quantity.
--
-- Throws an exception if the unit of the given quantity is not compatible with the receiver's unit.
--
-- ObjC selector: @- compare:@
compare_ :: (IsHKQuantity hkQuantity, IsHKQuantity quantity) => hkQuantity -> quantity -> IO NSComparisonResult
compare_ hkQuantity  quantity =
  withObjCPtr quantity $ \raw_quantity ->
      fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg hkQuantity (mkSelector "compare:") retCLong [argPtr (castPtr raw_quantity :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @quantityWithUnit:doubleValue:@
quantityWithUnit_doubleValueSelector :: Selector
quantityWithUnit_doubleValueSelector = mkSelector "quantityWithUnit:doubleValue:"

-- | @Selector@ for @isCompatibleWithUnit:@
isCompatibleWithUnitSelector :: Selector
isCompatibleWithUnitSelector = mkSelector "isCompatibleWithUnit:"

-- | @Selector@ for @doubleValueForUnit:@
doubleValueForUnitSelector :: Selector
doubleValueForUnitSelector = mkSelector "doubleValueForUnit:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

