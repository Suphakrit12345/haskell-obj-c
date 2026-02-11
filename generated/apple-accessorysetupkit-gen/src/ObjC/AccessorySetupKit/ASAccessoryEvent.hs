{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Event for status and other updates.
--
-- Generated bindings for @ASAccessoryEvent@.
module ObjC.AccessorySetupKit.ASAccessoryEvent
  ( ASAccessoryEvent
  , IsASAccessoryEvent(..)
  , init_
  , new
  , eventType
  , accessory
  , error_
  , initSelector
  , newSelector
  , eventTypeSelector
  , accessorySelector
  , errorSelector

  -- * Enum types
  , ASAccessoryEventType(ASAccessoryEventType)
  , pattern ASAccessoryEventTypeUnknown
  , pattern ASAccessoryEventTypeActivated
  , pattern ASAccessoryEventTypeInvalidated
  , pattern ASAccessoryEventTypeMigrationComplete
  , pattern ASAccessoryEventTypeAccessoryAdded
  , pattern ASAccessoryEventTypeAccessoryRemoved
  , pattern ASAccessoryEventTypeAccessoryChanged
  , pattern ASAccessoryEventTypeAccessoryDiscovered
  , pattern ASAccessoryEventTypePickerDidPresent
  , pattern ASAccessoryEventTypePickerDidDismiss
  , pattern ASAccessoryEventTypePickerSetupBridging
  , pattern ASAccessoryEventTypePickerSetupFailed
  , pattern ASAccessoryEventTypePickerSetupPairing
  , pattern ASAccessoryEventTypePickerSetupRename

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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessoryEvent)
init_ asAccessoryEvent  =
    sendMsg asAccessoryEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessoryEvent)
new asAccessoryEvent  =
    sendMsg asAccessoryEvent (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The type of event, such as accessory addition or removal, or picker presentation or removal.
--
-- Some event types may indicate that the event is a subclass of ``ASAccessoryEvent-c.class`` that provides additional properties.
--
-- ObjC selector: @- eventType@
eventType :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO ASAccessoryEventType
eventType asAccessoryEvent  =
    fmap (coerce :: CLong -> ASAccessoryEventType) $ sendMsg asAccessoryEvent (mkSelector "eventType") retCLong []

-- | The accessory involved in the event, if any.
--
-- The session populates this member for event types like ``ASAccessoryEventType/accessoryAdded`` and ``ASAccessoryEventType/accessoryChanged``, but not for life cycle or picker events like ``ASAccessoryEventType/activated`` or ``ASAccessoryEventType/pickerDidPresent``.
--
-- ObjC selector: @- accessory@
accessory :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessory)
accessory asAccessoryEvent  =
    sendMsg asAccessoryEvent (mkSelector "accessory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The error associated with the event, if any.
--
-- ObjC selector: @- error@
error_ :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id NSError)
error_ asAccessoryEvent  =
    sendMsg asAccessoryEvent (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector
eventTypeSelector = mkSelector "eventType"

-- | @Selector@ for @accessory@
accessorySelector :: Selector
accessorySelector = mkSelector "accessory"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

