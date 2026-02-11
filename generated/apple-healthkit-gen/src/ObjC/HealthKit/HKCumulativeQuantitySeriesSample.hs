{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKCumulativeQuantitySeriesSample@.
module ObjC.HealthKit.HKCumulativeQuantitySeriesSample
  ( HKCumulativeQuantitySeriesSample
  , IsHKCumulativeQuantitySeriesSample(..)
  , sum_
  , sumSelector


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

-- | @- sum@
sum_ :: IsHKCumulativeQuantitySeriesSample hkCumulativeQuantitySeriesSample => hkCumulativeQuantitySeriesSample -> IO (Id HKQuantity)
sum_ hkCumulativeQuantitySeriesSample  =
    sendMsg hkCumulativeQuantitySeriesSample (mkSelector "sum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sum@
sumSelector :: Selector
sumSelector = mkSelector "sum"

