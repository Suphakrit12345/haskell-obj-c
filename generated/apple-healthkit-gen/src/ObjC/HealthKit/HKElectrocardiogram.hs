{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKElectrocardiogram
--
-- An HKElectrocardiogram is a collection of voltage values as waveforms                from one or more leads
--
-- Generated bindings for @HKElectrocardiogram@.
module ObjC.HealthKit.HKElectrocardiogram
  ( HKElectrocardiogram
  , IsHKElectrocardiogram(..)
  , numberOfVoltageMeasurements
  , samplingFrequency
  , classification
  , averageHeartRate
  , symptomsStatus
  , numberOfVoltageMeasurementsSelector
  , samplingFrequencySelector
  , classificationSelector
  , averageHeartRateSelector
  , symptomsStatusSelector

  -- * Enum types
  , HKElectrocardiogramClassification(HKElectrocardiogramClassification)
  , pattern HKElectrocardiogramClassificationNotSet
  , pattern HKElectrocardiogramClassificationSinusRhythm
  , pattern HKElectrocardiogramClassificationAtrialFibrillation
  , pattern HKElectrocardiogramClassificationInconclusiveLowHeartRate
  , pattern HKElectrocardiogramClassificationInconclusiveHighHeartRate
  , pattern HKElectrocardiogramClassificationInconclusivePoorReading
  , pattern HKElectrocardiogramClassificationInconclusiveOther
  , pattern HKElectrocardiogramClassificationUnrecognized
  , HKElectrocardiogramSymptomsStatus(HKElectrocardiogramSymptomsStatus)
  , pattern HKElectrocardiogramSymptomsStatusNotSet
  , pattern HKElectrocardiogramSymptomsStatusNone
  , pattern HKElectrocardiogramSymptomsStatusPresent

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

-- | The number of voltage measurements in the electrocardiogram.
--
-- ObjC selector: @- numberOfVoltageMeasurements@
numberOfVoltageMeasurements :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO CLong
numberOfVoltageMeasurements hkElectrocardiogram  =
    sendMsg hkElectrocardiogram (mkSelector "numberOfVoltageMeasurements") retCLong []

-- | The frequency at which the data was sampled. This is reported in [HKUnit hertzUnit].
--
-- ObjC selector: @- samplingFrequency@
samplingFrequency :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO (Id HKQuantity)
samplingFrequency hkElectrocardiogram  =
    sendMsg hkElectrocardiogram (mkSelector "samplingFrequency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The classification of this electrocardiogram sample.
--
-- ObjC selector: @- classification@
classification :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO HKElectrocardiogramClassification
classification hkElectrocardiogram  =
    fmap (coerce :: CLong -> HKElectrocardiogramClassification) $ sendMsg hkElectrocardiogram (mkSelector "classification") retCLong []

-- | The average heart rate of the user while the electrocardiogram was recorded.
--
-- ObjC selector: @- averageHeartRate@
averageHeartRate :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO (Id HKQuantity)
averageHeartRate hkElectrocardiogram  =
    sendMsg hkElectrocardiogram (mkSelector "averageHeartRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the user experienced symptoms during this electrocardiogram.
--
-- ObjC selector: @- symptomsStatus@
symptomsStatus :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO HKElectrocardiogramSymptomsStatus
symptomsStatus hkElectrocardiogram  =
    fmap (coerce :: CLong -> HKElectrocardiogramSymptomsStatus) $ sendMsg hkElectrocardiogram (mkSelector "symptomsStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfVoltageMeasurements@
numberOfVoltageMeasurementsSelector :: Selector
numberOfVoltageMeasurementsSelector = mkSelector "numberOfVoltageMeasurements"

-- | @Selector@ for @samplingFrequency@
samplingFrequencySelector :: Selector
samplingFrequencySelector = mkSelector "samplingFrequency"

-- | @Selector@ for @classification@
classificationSelector :: Selector
classificationSelector = mkSelector "classification"

-- | @Selector@ for @averageHeartRate@
averageHeartRateSelector :: Selector
averageHeartRateSelector = mkSelector "averageHeartRate"

-- | @Selector@ for @symptomsStatus@
symptomsStatusSelector :: Selector
symptomsStatusSelector = mkSelector "symptomsStatus"

