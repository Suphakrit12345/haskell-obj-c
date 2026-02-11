{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a single measurement relative to a DL-TDOA anchor.
--
-- Generated bindings for @NIDLTDOAMeasurement@.
module ObjC.NearbyInteraction.NIDLTDOAMeasurement
  ( NIDLTDOAMeasurement
  , IsNIDLTDOAMeasurement(..)
  , init_
  , new
  , address
  , measurementType
  , transmitTime
  , receiveTime
  , signalStrength
  , carrierFrequencyOffset
  , coordinatesType
  , initSelector
  , newSelector
  , addressSelector
  , measurementTypeSelector
  , transmitTimeSelector
  , receiveTimeSelector
  , signalStrengthSelector
  , carrierFrequencyOffsetSelector
  , coordinatesTypeSelector

  -- * Enum types
  , NIDLTDOACoordinatesType(NIDLTDOACoordinatesType)
  , pattern NIDLTDOACoordinatesTypeGeodetic
  , pattern NIDLTDOACoordinatesTypeRelative
  , NIDLTDOAMeasurementType(NIDLTDOAMeasurementType)
  , pattern NIDLTDOAMeasurementTypePoll
  , pattern NIDLTDOAMeasurementTypeResponse
  , pattern NIDLTDOAMeasurementTypeFinal

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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.NearbyInteraction.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO (Id NIDLTDOAMeasurement)
init_ nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NIDLTDOAMeasurement)
new  =
  do
    cls' <- getRequiredClass "NIDLTDOAMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates the address of anchor of this measurement.
--
-- ObjC selector: @- address@
address :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CULong
address nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "address") retCULong []

-- | Indicates the type of this measurement.
--
-- ObjC selector: @- measurementType@
measurementType :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO NIDLTDOAMeasurementType
measurementType nidltdoaMeasurement  =
    fmap (coerce :: CLong -> NIDLTDOAMeasurementType) $ sendMsg nidltdoaMeasurement (mkSelector "measurementType") retCLong []

-- | Indicates the transmission timestamp (in seconds).
--
-- ObjC selector: @- transmitTime@
transmitTime :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
transmitTime nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "transmitTime") retCDouble []

-- | Indicates the reception timestamp (in seconds).
--
-- ObjC selector: @- receiveTime@
receiveTime :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
receiveTime nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "receiveTime") retCDouble []

-- | Indicates the signal strength in dBm.
--
-- ObjC selector: @- signalStrength@
signalStrength :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
signalStrength nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "signalStrength") retCDouble []

-- | Indicates the estimated carrier frequency offset (dimensionless).
--
-- ObjC selector: @- carrierFrequencyOffset@
carrierFrequencyOffset :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
carrierFrequencyOffset nidltdoaMeasurement  =
    sendMsg nidltdoaMeasurement (mkSelector "carrierFrequencyOffset") retCDouble []

-- | Inidicates the type of coordinates of this anchor.
--
-- ObjC selector: @- coordinatesType@
coordinatesType :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO NIDLTDOACoordinatesType
coordinatesType nidltdoaMeasurement  =
    fmap (coerce :: CLong -> NIDLTDOACoordinatesType) $ sendMsg nidltdoaMeasurement (mkSelector "coordinatesType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @transmitTime@
transmitTimeSelector :: Selector
transmitTimeSelector = mkSelector "transmitTime"

-- | @Selector@ for @receiveTime@
receiveTimeSelector :: Selector
receiveTimeSelector = mkSelector "receiveTime"

-- | @Selector@ for @signalStrength@
signalStrengthSelector :: Selector
signalStrengthSelector = mkSelector "signalStrength"

-- | @Selector@ for @carrierFrequencyOffset@
carrierFrequencyOffsetSelector :: Selector
carrierFrequencyOffsetSelector = mkSelector "carrierFrequencyOffset"

-- | @Selector@ for @coordinatesType@
coordinatesTypeSelector :: Selector
coordinatesTypeSelector = mkSelector "coordinatesType"

