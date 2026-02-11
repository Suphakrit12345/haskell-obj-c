{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAmbientLightSample@.
module ObjC.SensorKit.SRAmbientLightSample
  ( SRAmbientLightSample
  , IsSRAmbientLightSample(..)
  , placement
  , lux
  , placementSelector
  , luxSelector

  -- * Enum types
  , SRAmbientLightSensorPlacement(SRAmbientLightSensorPlacement)
  , pattern SRAmbientLightSensorPlacementUnknown
  , pattern SRAmbientLightSensorPlacementFrontTop
  , pattern SRAmbientLightSensorPlacementFrontBottom
  , pattern SRAmbientLightSensorPlacementFrontRight
  , pattern SRAmbientLightSensorPlacementFrontLeft
  , pattern SRAmbientLightSensorPlacementFrontTopRight
  , pattern SRAmbientLightSensorPlacementFrontTopLeft
  , pattern SRAmbientLightSensorPlacementFrontBottomRight
  , pattern SRAmbientLightSensorPlacementFrontBottomLeft

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
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- placement@
placement :: IsSRAmbientLightSample srAmbientLightSample => srAmbientLightSample -> IO SRAmbientLightSensorPlacement
placement srAmbientLightSample  =
    fmap (coerce :: CLong -> SRAmbientLightSensorPlacement) $ sendMsg srAmbientLightSample (mkSelector "placement") retCLong []

-- | @- lux@
lux :: IsSRAmbientLightSample srAmbientLightSample => srAmbientLightSample -> IO (Id NSMeasurement)
lux srAmbientLightSample  =
    sendMsg srAmbientLightSample (mkSelector "lux") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @placement@
placementSelector :: Selector
placementSelector = mkSelector "placement"

-- | @Selector@ for @lux@
luxSelector :: Selector
luxSelector = mkSelector "lux"

