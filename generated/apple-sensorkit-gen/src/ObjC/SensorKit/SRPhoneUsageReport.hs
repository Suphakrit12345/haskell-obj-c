{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhoneUsageReport@.
module ObjC.SensorKit.SRPhoneUsageReport
  ( SRPhoneUsageReport
  , IsSRPhoneUsageReport(..)
  , duration
  , totalOutgoingCalls
  , totalIncomingCalls
  , totalUniqueContacts
  , totalPhoneCallDuration
  , durationSelector
  , totalOutgoingCallsSelector
  , totalIncomingCallsSelector
  , totalUniqueContactsSelector
  , totalPhoneCallDurationSelector


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

-- | @- duration@
duration :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CDouble
duration srPhoneUsageReport  =
    sendMsg srPhoneUsageReport (mkSelector "duration") retCDouble []

-- | @- totalOutgoingCalls@
totalOutgoingCalls :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalOutgoingCalls srPhoneUsageReport  =
    sendMsg srPhoneUsageReport (mkSelector "totalOutgoingCalls") retCLong []

-- | @- totalIncomingCalls@
totalIncomingCalls :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalIncomingCalls srPhoneUsageReport  =
    sendMsg srPhoneUsageReport (mkSelector "totalIncomingCalls") retCLong []

-- | @- totalUniqueContacts@
totalUniqueContacts :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CLong
totalUniqueContacts srPhoneUsageReport  =
    sendMsg srPhoneUsageReport (mkSelector "totalUniqueContacts") retCLong []

-- | @- totalPhoneCallDuration@
totalPhoneCallDuration :: IsSRPhoneUsageReport srPhoneUsageReport => srPhoneUsageReport -> IO CDouble
totalPhoneCallDuration srPhoneUsageReport  =
    sendMsg srPhoneUsageReport (mkSelector "totalPhoneCallDuration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @totalOutgoingCalls@
totalOutgoingCallsSelector :: Selector
totalOutgoingCallsSelector = mkSelector "totalOutgoingCalls"

-- | @Selector@ for @totalIncomingCalls@
totalIncomingCallsSelector :: Selector
totalIncomingCallsSelector = mkSelector "totalIncomingCalls"

-- | @Selector@ for @totalUniqueContacts@
totalUniqueContactsSelector :: Selector
totalUniqueContactsSelector = mkSelector "totalUniqueContacts"

-- | @Selector@ for @totalPhoneCallDuration@
totalPhoneCallDurationSelector :: Selector
totalPhoneCallDurationSelector = mkSelector "totalPhoneCallDuration"

