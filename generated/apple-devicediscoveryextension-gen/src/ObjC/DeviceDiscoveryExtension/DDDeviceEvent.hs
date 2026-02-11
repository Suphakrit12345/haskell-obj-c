{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Device-related event (e.g. found, lost).
--
-- Generated bindings for @DDDeviceEvent@.
module ObjC.DeviceDiscoveryExtension.DDDeviceEvent
  ( DDDeviceEvent
  , IsDDDeviceEvent(..)
  , initWithEventType_device
  , device
  , eventType
  , initWithEventType_deviceSelector
  , deviceSelector
  , eventTypeSelector

  -- * Enum types
  , DDEventType(DDEventType)
  , pattern DDEventTypeUnknown
  , pattern DDEventTypeDeviceFound
  , pattern DDEventTypeDeviceLost
  , pattern DDEventTypeDeviceChanged

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

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.DeviceDiscoveryExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a device event.
--
-- ObjC selector: @- initWithEventType:device:@
initWithEventType_device :: (IsDDDeviceEvent ddDeviceEvent, IsDDDevice device) => ddDeviceEvent -> DDEventType -> device -> IO (Id DDDeviceEvent)
initWithEventType_device ddDeviceEvent  type_ device =
  withObjCPtr device $ \raw_device ->
      sendMsg ddDeviceEvent (mkSelector "initWithEventType:device:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | Device found or lost.
--
-- ObjC selector: @- device@
device :: IsDDDeviceEvent ddDeviceEvent => ddDeviceEvent -> IO (Id DDDevice)
device ddDeviceEvent  =
    sendMsg ddDeviceEvent (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Type of event.
--
-- ObjC selector: @- eventType@
eventType :: IsDDDeviceEvent ddDeviceEvent => ddDeviceEvent -> IO DDEventType
eventType ddDeviceEvent  =
    fmap (coerce :: CLong -> DDEventType) $ sendMsg ddDeviceEvent (mkSelector "eventType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEventType:device:@
initWithEventType_deviceSelector :: Selector
initWithEventType_deviceSelector = mkSelector "initWithEventType:device:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector
eventTypeSelector = mkSelector "eventType"

