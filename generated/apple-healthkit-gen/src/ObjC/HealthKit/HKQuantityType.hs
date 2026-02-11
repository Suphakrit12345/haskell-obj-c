{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantityType
--
-- Represents types of HKQuantitySamples.
--
-- Generated bindings for @HKQuantityType@.
module ObjC.HealthKit.HKQuantityType
  ( HKQuantityType
  , IsHKQuantityType(..)
  , isCompatibleWithUnit
  , aggregationStyle
  , isCompatibleWithUnitSelector
  , aggregationStyleSelector

  -- * Enum types
  , HKQuantityAggregationStyle(HKQuantityAggregationStyle)
  , pattern HKQuantityAggregationStyleCumulative
  , pattern HKQuantityAggregationStyleDiscreteArithmetic
  , pattern HKQuantityAggregationStyleDiscrete
  , pattern HKQuantityAggregationStyleDiscreteTemporallyWeighted
  , pattern HKQuantityAggregationStyleDiscreteEquivalentContinuousLevel

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
import ObjC.Foundation.Internal.Classes

-- | isCompatibleWithUnit:
--
-- Returns YES if the type of HKQuantitySample represented by the receiver can be created with quantities                 of the given unit.
--
-- ObjC selector: @- isCompatibleWithUnit:@
isCompatibleWithUnit :: (IsHKQuantityType hkQuantityType, IsHKUnit unit) => hkQuantityType -> unit -> IO Bool
isCompatibleWithUnit hkQuantityType  unit =
  withObjCPtr unit $ \raw_unit ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkQuantityType (mkSelector "isCompatibleWithUnit:") retCULong [argPtr (castPtr raw_unit :: Ptr ())]

-- | @- aggregationStyle@
aggregationStyle :: IsHKQuantityType hkQuantityType => hkQuantityType -> IO HKQuantityAggregationStyle
aggregationStyle hkQuantityType  =
    fmap (coerce :: CLong -> HKQuantityAggregationStyle) $ sendMsg hkQuantityType (mkSelector "aggregationStyle") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isCompatibleWithUnit:@
isCompatibleWithUnitSelector :: Selector
isCompatibleWithUnitSelector = mkSelector "isCompatibleWithUnit:"

-- | @Selector@ for @aggregationStyle@
aggregationStyleSelector :: Selector
aggregationStyleSelector = mkSelector "aggregationStyle"

