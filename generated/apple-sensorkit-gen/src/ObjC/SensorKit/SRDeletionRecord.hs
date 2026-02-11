{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRDeletionRecord@.
module ObjC.SensorKit.SRDeletionRecord
  ( SRDeletionRecord
  , IsSRDeletionRecord(..)
  , startTime
  , endTime
  , reason
  , startTimeSelector
  , endTimeSelector
  , reasonSelector

  -- * Enum types
  , SRDeletionReason(SRDeletionReason)
  , pattern SRDeletionReasonUserInitiated
  , pattern SRDeletionReasonLowDiskSpace
  , pattern SRDeletionReasonAgeLimit
  , pattern SRDeletionReasonNoInterestedClients
  , pattern SRDeletionReasonSystemInitiated

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

-- | @- startTime@
startTime :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO CDouble
startTime srDeletionRecord  =
    sendMsg srDeletionRecord (mkSelector "startTime") retCDouble []

-- | @- endTime@
endTime :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO CDouble
endTime srDeletionRecord  =
    sendMsg srDeletionRecord (mkSelector "endTime") retCDouble []

-- | @- reason@
reason :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO SRDeletionReason
reason srDeletionRecord  =
    fmap (coerce :: CLong -> SRDeletionReason) $ sendMsg srDeletionRecord (mkSelector "reason") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

