{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRKeyboardProbabilityMetric@.
module ObjC.SensorKit.SRKeyboardProbabilityMetric
  ( SRKeyboardProbabilityMetric
  , IsSRKeyboardProbabilityMetric(..)
  , distributionSampleValues
  , distributionSampleValuesSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sample values from probability distribution
--
-- ObjC selector: @- distributionSampleValues@
distributionSampleValues :: IsSRKeyboardProbabilityMetric srKeyboardProbabilityMetric => srKeyboardProbabilityMetric -> IO (Id NSArray)
distributionSampleValues srKeyboardProbabilityMetric  =
    sendMsg srKeyboardProbabilityMetric (mkSelector "distributionSampleValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distributionSampleValues@
distributionSampleValuesSelector :: Selector
distributionSampleValuesSelector = mkSelector "distributionSampleValues"

