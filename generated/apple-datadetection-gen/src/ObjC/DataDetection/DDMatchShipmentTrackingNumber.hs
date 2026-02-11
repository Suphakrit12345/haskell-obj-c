{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains parcel tracking information that the data detection system matches.
--
-- The DataDetection framework returns a shipment tracking number match in a @DDMatchShipmentTrackingNumber@ object, which contains a carrier name and tracking identifier.
--
-- Generated bindings for @DDMatchShipmentTrackingNumber@.
module ObjC.DataDetection.DDMatchShipmentTrackingNumber
  ( DDMatchShipmentTrackingNumber
  , IsDDMatchShipmentTrackingNumber(..)
  , carrier
  , trackingNumber
  , carrierSelector
  , trackingNumberSelector


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

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of a parcel carrier.
--
-- ObjC selector: @- carrier@
carrier :: IsDDMatchShipmentTrackingNumber ddMatchShipmentTrackingNumber => ddMatchShipmentTrackingNumber -> IO (Id NSString)
carrier ddMatchShipmentTrackingNumber  =
    sendMsg ddMatchShipmentTrackingNumber (mkSelector "carrier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A string that represents a carrier’s tracking identifier for a parcel.
--
-- ObjC selector: @- trackingNumber@
trackingNumber :: IsDDMatchShipmentTrackingNumber ddMatchShipmentTrackingNumber => ddMatchShipmentTrackingNumber -> IO (Id NSString)
trackingNumber ddMatchShipmentTrackingNumber  =
    sendMsg ddMatchShipmentTrackingNumber (mkSelector "trackingNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @carrier@
carrierSelector :: Selector
carrierSelector = mkSelector "carrier"

-- | @Selector@ for @trackingNumber@
trackingNumberSelector :: Selector
trackingNumberSelector = mkSelector "trackingNumber"

