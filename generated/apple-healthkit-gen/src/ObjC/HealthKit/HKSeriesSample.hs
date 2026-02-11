{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSeriesSample
--
-- This class represents a type of HKSample that references a series of data.
--
-- Generated bindings for @HKSeriesSample@.
module ObjC.HealthKit.HKSeriesSample
  ( HKSeriesSample
  , IsHKSeriesSample(..)
  , count
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

-- | count
--
-- The number of individual series datum represented by the receiver and accessible                through the appropriate HKQuery series subclass.
--
-- ObjC selector: @- count@
count :: IsHKSeriesSample hkSeriesSample => hkSeriesSample -> IO CULong
count hkSeriesSample  =
    sendMsg hkSeriesSample (mkSelector "count") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

