{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWristDetection@.
module ObjC.SensorKit.SRWristDetection
  ( SRWristDetection
  , IsSRWristDetection(..)
  , onWrist
  , wristLocation
  , crownOrientation
  , onWristSelector
  , wristLocationSelector
  , crownOrientationSelector

  -- * Enum types
  , SRCrownOrientation(SRCrownOrientation)
  , pattern SRCrownOrientationLeft
  , pattern SRCrownOrientationRight
  , SRWristLocation(SRWristLocation)
  , pattern SRWristLocationLeft
  , pattern SRWristLocationRight

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

-- | @- onWrist@
onWrist :: IsSRWristDetection srWristDetection => srWristDetection -> IO Bool
onWrist srWristDetection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srWristDetection (mkSelector "onWrist") retCULong []

-- | @- wristLocation@
wristLocation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRWristLocation
wristLocation srWristDetection  =
    fmap (coerce :: CLong -> SRWristLocation) $ sendMsg srWristDetection (mkSelector "wristLocation") retCLong []

-- | @- crownOrientation@
crownOrientation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRCrownOrientation
crownOrientation srWristDetection  =
    fmap (coerce :: CLong -> SRCrownOrientation) $ sendMsg srWristDetection (mkSelector "crownOrientation") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @onWrist@
onWristSelector :: Selector
onWristSelector = mkSelector "onWrist"

-- | @Selector@ for @wristLocation@
wristLocationSelector :: Selector
wristLocationSelector = mkSelector "wristLocation"

-- | @Selector@ for @crownOrientation@
crownOrientationSelector :: Selector
crownOrientationSelector = mkSelector "crownOrientation"

