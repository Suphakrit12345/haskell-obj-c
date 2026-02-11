{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneStoppedEvent@.
module ObjC.Matter.MTRZoneManagementClusterZoneStoppedEvent
  ( MTRZoneManagementClusterZoneStoppedEvent
  , IsMTRZoneManagementClusterZoneStoppedEvent(..)
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
zone :: IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent => mtrZoneManagementClusterZoneStoppedEvent -> IO (Id NSNumber)
zone mtrZoneManagementClusterZoneStoppedEvent  =
    sendMsg mtrZoneManagementClusterZoneStoppedEvent (mkSelector "zone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent, IsNSNumber value) => mtrZoneManagementClusterZoneStoppedEvent -> value -> IO ()
setZone mtrZoneManagementClusterZoneStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneStoppedEvent (mkSelector "setZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reason@
reason :: IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent => mtrZoneManagementClusterZoneStoppedEvent -> IO (Id NSNumber)
reason mtrZoneManagementClusterZoneStoppedEvent  =
    sendMsg mtrZoneManagementClusterZoneStoppedEvent (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReason:@
setReason :: (IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent, IsNSNumber value) => mtrZoneManagementClusterZoneStoppedEvent -> value -> IO ()
setReason mtrZoneManagementClusterZoneStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterZoneStoppedEvent (mkSelector "setReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

