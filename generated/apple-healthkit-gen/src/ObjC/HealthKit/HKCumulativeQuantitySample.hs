{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCumulativeQuantitySample
--
-- An HKQuantitySample subclass representing a quantity measurement with cumulative aggregation style.
--
-- Generated bindings for @HKCumulativeQuantitySample@.
module ObjC.HealthKit.HKCumulativeQuantitySample
  ( HKCumulativeQuantitySample
  , IsHKCumulativeQuantitySample(..)
  , sumQuantity
  , sumQuantitySelector


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

-- | sumQuantity
--
-- The sum of quantities represented by the receiver.
--
-- ObjC selector: @- sumQuantity@
sumQuantity :: IsHKCumulativeQuantitySample hkCumulativeQuantitySample => hkCumulativeQuantitySample -> IO (Id HKQuantity)
sumQuantity hkCumulativeQuantitySample  =
    sendMsg hkCumulativeQuantitySample (mkSelector "sumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sumQuantity@
sumQuantitySelector :: Selector
sumQuantitySelector = mkSelector "sumQuantity"

