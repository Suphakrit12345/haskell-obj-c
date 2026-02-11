{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRApplicationUsage@.
module ObjC.SensorKit.SRApplicationUsage
  ( SRApplicationUsage
  , IsSRApplicationUsage(..)
  , bundleIdentifier
  , usageTime
  , relativeStartTime
  , bundleIdentifierSelector
  , usageTimeSelector
  , relativeStartTimeSelector


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

-- | The bundle identifier of the app in use. Only populated for Apple apps.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSString)
bundleIdentifier srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The amount of time the app is used
--
-- ObjC selector: @- usageTime@
usageTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
usageTime srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "usageTime") retCDouble []

-- | relativeStartTime
--
-- App start time relative to the first app start time in the report interval
--
-- relativeStartTime value for the very first app in the report interval is equal to 0, N seconds for the seccond app and so on. This will allow to order app uses and determine the time between app uses.
--
-- ObjC selector: @- relativeStartTime@
relativeStartTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
relativeStartTime srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "relativeStartTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @usageTime@
usageTimeSelector :: Selector
usageTimeSelector = mkSelector "usageTime"

-- | @Selector@ for @relativeStartTime@
relativeStartTimeSelector :: Selector
relativeStartTimeSelector = mkSelector "relativeStartTime"

