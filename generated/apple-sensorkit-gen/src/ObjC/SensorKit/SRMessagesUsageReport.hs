{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRMessagesUsageReport@.
module ObjC.SensorKit.SRMessagesUsageReport
  ( SRMessagesUsageReport
  , IsSRMessagesUsageReport(..)
  , duration
  , totalOutgoingMessages
  , totalIncomingMessages
  , totalUniqueContacts
  , durationSelector
  , totalOutgoingMessagesSelector
  , totalIncomingMessagesSelector
  , totalUniqueContactsSelector


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
duration :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CDouble
duration srMessagesUsageReport  =
    sendMsg srMessagesUsageReport (mkSelector "duration") retCDouble []

-- | @- totalOutgoingMessages@
totalOutgoingMessages :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalOutgoingMessages srMessagesUsageReport  =
    sendMsg srMessagesUsageReport (mkSelector "totalOutgoingMessages") retCLong []

-- | @- totalIncomingMessages@
totalIncomingMessages :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalIncomingMessages srMessagesUsageReport  =
    sendMsg srMessagesUsageReport (mkSelector "totalIncomingMessages") retCLong []

-- | @- totalUniqueContacts@
totalUniqueContacts :: IsSRMessagesUsageReport srMessagesUsageReport => srMessagesUsageReport -> IO CLong
totalUniqueContacts srMessagesUsageReport  =
    sendMsg srMessagesUsageReport (mkSelector "totalUniqueContacts") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @totalOutgoingMessages@
totalOutgoingMessagesSelector :: Selector
totalOutgoingMessagesSelector = mkSelector "totalOutgoingMessages"

-- | @Selector@ for @totalIncomingMessages@
totalIncomingMessagesSelector :: Selector
totalIncomingMessagesSelector = mkSelector "totalIncomingMessages"

-- | @Selector@ for @totalUniqueContacts@
totalUniqueContactsSelector :: Selector
totalUniqueContactsSelector = mkSelector "totalUniqueContacts"

