{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDiscreteQuantitySample
--
-- An HKQuantitySample subclass representing a quantity measurement with                discrete aggregation style.
--
-- Generated bindings for @HKDiscreteQuantitySample@.
module ObjC.HealthKit.HKDiscreteQuantitySample
  ( HKDiscreteQuantitySample
  , IsHKDiscreteQuantitySample(..)
  , minimumQuantity
  , averageQuantity
  , maximumQuantity
  , mostRecentQuantity
  , mostRecentQuantityDateInterval
  , minimumQuantitySelector
  , averageQuantitySelector
  , maximumQuantitySelector
  , mostRecentQuantitySelector
  , mostRecentQuantityDateIntervalSelector


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

-- | minimumQuantity
--
-- The minimum of the receiver's quantities
--
-- ObjC selector: @- minimumQuantity@
minimumQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
minimumQuantity hkDiscreteQuantitySample  =
    sendMsg hkDiscreteQuantitySample (mkSelector "minimumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | averageQuantity
--
-- The average of the receiver's quantities
--
-- ObjC selector: @- averageQuantity@
averageQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
averageQuantity hkDiscreteQuantitySample  =
    sendMsg hkDiscreteQuantitySample (mkSelector "averageQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maximumQuantity
--
-- The maximum of the receiver's quantities
--
-- ObjC selector: @- maximumQuantity@
maximumQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
maximumQuantity hkDiscreteQuantitySample  =
    sendMsg hkDiscreteQuantitySample (mkSelector "maximumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mostRecentQuantity
--
-- The receiver's quantity with most recent date interval
--
-- ObjC selector: @- mostRecentQuantity@
mostRecentQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
mostRecentQuantity hkDiscreteQuantitySample  =
    sendMsg hkDiscreteQuantitySample (mkSelector "mostRecentQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mostRecentQuantityDateInterval
--
-- The date interval for the receiver's most recent quantity
--
-- ObjC selector: @- mostRecentQuantityDateInterval@
mostRecentQuantityDateInterval :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id NSDateInterval)
mostRecentQuantityDateInterval hkDiscreteQuantitySample  =
    sendMsg hkDiscreteQuantitySample (mkSelector "mostRecentQuantityDateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumQuantity@
minimumQuantitySelector :: Selector
minimumQuantitySelector = mkSelector "minimumQuantity"

-- | @Selector@ for @averageQuantity@
averageQuantitySelector :: Selector
averageQuantitySelector = mkSelector "averageQuantity"

-- | @Selector@ for @maximumQuantity@
maximumQuantitySelector :: Selector
maximumQuantitySelector = mkSelector "maximumQuantity"

-- | @Selector@ for @mostRecentQuantity@
mostRecentQuantitySelector :: Selector
mostRecentQuantitySelector = mkSelector "mostRecentQuantity"

-- | @Selector@ for @mostRecentQuantityDateInterval@
mostRecentQuantityDateIntervalSelector :: Selector
mostRecentQuantityDateIntervalSelector = mkSelector "mostRecentQuantityDateInterval"

