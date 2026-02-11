{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWebUsage@.
module ObjC.SensorKit.SRWebUsage
  ( SRWebUsage
  , IsSRWebUsage(..)
  , totalUsageTime
  , totalUsageTimeSelector


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

-- | @- totalUsageTime@
totalUsageTime :: IsSRWebUsage srWebUsage => srWebUsage -> IO CDouble
totalUsageTime srWebUsage  =
    sendMsg srWebUsage (mkSelector "totalUsageTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalUsageTime@
totalUsageTimeSelector :: Selector
totalUsageTimeSelector = mkSelector "totalUsageTime"

