{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKElectrocardiogramVoltageMeasurement
--
-- An HKElectrocardiogramVoltageMeasurement contains voltage quantities for all leads at a single instance of measurement.
--
-- Each HKElectrocardiogramVoltageMeasurement object corresponds to the voltage quantities across all leads for a given instance in time.
--
-- Generated bindings for @HKElectrocardiogramVoltageMeasurement@.
module ObjC.HealthKit.HKElectrocardiogramVoltageMeasurement
  ( HKElectrocardiogramVoltageMeasurement
  , IsHKElectrocardiogramVoltageMeasurement(..)
  , quantityForLead
  , timeSinceSampleStart
  , quantityForLeadSelector
  , timeSinceSampleStartSelector

  -- * Enum types
  , HKElectrocardiogramLead(HKElectrocardiogramLead)
  , pattern HKElectrocardiogramLeadAppleWatchSimilarToLeadI

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

-- | quantityForLead:
--
-- Returns an HKQuantity for the specified lead with a unit compatible with [HKUnit voltUnit].
--
-- @lead@ — The HKElectrocardiogramLead for which voltage quantity will be returned.
--
-- ObjC selector: @- quantityForLead:@
quantityForLead :: IsHKElectrocardiogramVoltageMeasurement hkElectrocardiogramVoltageMeasurement => hkElectrocardiogramVoltageMeasurement -> HKElectrocardiogramLead -> IO (Id HKQuantity)
quantityForLead hkElectrocardiogramVoltageMeasurement  lead =
    sendMsg hkElectrocardiogramVoltageMeasurement (mkSelector "quantityForLead:") (retPtr retVoid) [argCLong (coerce lead)] >>= retainedObject . castPtr

-- | The time interval between this voltage measurement and the start of the sample.
--
-- ObjC selector: @- timeSinceSampleStart@
timeSinceSampleStart :: IsHKElectrocardiogramVoltageMeasurement hkElectrocardiogramVoltageMeasurement => hkElectrocardiogramVoltageMeasurement -> IO CDouble
timeSinceSampleStart hkElectrocardiogramVoltageMeasurement  =
    sendMsg hkElectrocardiogramVoltageMeasurement (mkSelector "timeSinceSampleStart") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quantityForLead:@
quantityForLeadSelector :: Selector
quantityForLeadSelector = mkSelector "quantityForLead:"

-- | @Selector@ for @timeSinceSampleStart@
timeSinceSampleStartSelector :: Selector
timeSinceSampleStartSelector = mkSelector "timeSinceSampleStart"

