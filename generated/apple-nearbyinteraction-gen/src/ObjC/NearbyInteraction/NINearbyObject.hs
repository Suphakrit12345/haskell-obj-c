{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A nearby object with distance and direction measurements.
--
-- Generated bindings for @NINearbyObject@.
module ObjC.NearbyInteraction.NINearbyObject
  ( NINearbyObject
  , IsNINearbyObject(..)
  , init_
  , new
  , discoveryToken
  , distance
  , verticalDirectionEstimate
  , horizontalAngle
  , initSelector
  , newSelector
  , discoveryTokenSelector
  , distanceSelector
  , verticalDirectionEstimateSelector
  , horizontalAngleSelector

  -- * Enum types
  , NINearbyObjectVerticalDirectionEstimate(NINearbyObjectVerticalDirectionEstimate)
  , pattern NINearbyObjectVerticalDirectionEstimateUnknown
  , pattern NINearbyObjectVerticalDirectionEstimateSame
  , pattern NINearbyObjectVerticalDirectionEstimateAbove
  , pattern NINearbyObjectVerticalDirectionEstimateBelow
  , pattern NINearbyObjectVerticalDirectionEstimateAboveOrBelow

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
init_ :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO (Id NINearbyObject)
init_ niNearbyObject  =
    sendMsg niNearbyObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NINearbyObject)
new  =
  do
    cls' <- getRequiredClass "NINearbyObject"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Nearby interaction discovery token
--
-- This discovery token will be equal to the token provided in the configuration with which the session was run.
--
-- ObjC selector: @- discoveryToken@
discoveryToken :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO (Id NIDiscoveryToken)
discoveryToken niNearbyObject  =
    sendMsg niNearbyObject (mkSelector "discoveryToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Distance to the nearby object in meters. If not available in this update, the value of this property will be equal to NINearbyObjectDistanceNotAvailable in Objective C, or nil in Swift.
--
-- ObjC selector: @- distance@
distance :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO CFloat
distance niNearbyObject  =
    sendMsg niNearbyObject (mkSelector "distance") retCFloat []

-- | An indication of the positional relationship to the nearby object in the vertical dimension.
--
-- ObjC selector: @- verticalDirectionEstimate@
verticalDirectionEstimate :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO NINearbyObjectVerticalDirectionEstimate
verticalDirectionEstimate niNearbyObject  =
    fmap (coerce :: CLong -> NINearbyObjectVerticalDirectionEstimate) $ sendMsg niNearbyObject (mkSelector "verticalDirectionEstimate") retCLong []

-- | An angle in radians indicating the azimuthal direction to the nearby object.
--
-- when unavailable, the value will be set to @NINearbyObjectAngleNotAvailable@.
--
-- ObjC selector: @- horizontalAngle@
horizontalAngle :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO CFloat
horizontalAngle niNearbyObject  =
    sendMsg niNearbyObject (mkSelector "horizontalAngle") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @discoveryToken@
discoveryTokenSelector :: Selector
discoveryTokenSelector = mkSelector "discoveryToken"

-- | @Selector@ for @distance@
distanceSelector :: Selector
distanceSelector = mkSelector "distance"

-- | @Selector@ for @verticalDirectionEstimate@
verticalDirectionEstimateSelector :: Selector
verticalDirectionEstimateSelector = mkSelector "verticalDirectionEstimate"

-- | @Selector@ for @horizontalAngle@
horizontalAngleSelector :: Selector
horizontalAngleSelector = mkSelector "horizontalAngle"

