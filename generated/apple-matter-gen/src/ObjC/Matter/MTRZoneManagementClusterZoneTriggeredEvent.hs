{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneTriggeredEvent@.
module ObjC.Matter.MTRZoneManagementClusterZoneTriggeredEvent
  ( MTRZoneManagementClusterZoneTriggeredEvent
  , IsMTRZoneManagementClusterZoneTriggeredEvent(..)
  , zone
  , setZone
  , reason
  , setReason
  , zoneSelector
  , setZoneSelector
  , reasonSelector
  , setReasonSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zone@
zone :: IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent => mtrZoneManagementClusterZoneTriggeredEvent -> IO (Id NSNumber)
zone mtrZoneManagementClusterZoneTriggeredEvent  =
    sendMsg mtrZoneManagementClusterZoneTriggeredEvent (mkSelector "zone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent, IsNSNumber value) => mtrZoneManagementClusterZoneTriggeredEvent -> value -> IO ()
setZone mtrZoneManagementClusterZoneTriggeredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggeredEvent (mkSelector "setZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reason@
reason :: IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent => mtrZoneManagementClusterZoneTriggeredEvent -> IO (Id NSNumber)
reason mtrZoneManagementClusterZoneTriggeredEvent  =
    sendMsg mtrZoneManagementClusterZoneTriggeredEvent (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReason:@
setReason :: (IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent, IsNSNumber value) => mtrZoneManagementClusterZoneTriggeredEvent -> value -> IO ()
setReason mtrZoneManagementClusterZoneTriggeredEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneTriggeredEvent (mkSelector "setReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zone@
zoneSelector :: Selector
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector
setZoneSelector = mkSelector "setZone:"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector
setReasonSelector = mkSelector "setReason:"

