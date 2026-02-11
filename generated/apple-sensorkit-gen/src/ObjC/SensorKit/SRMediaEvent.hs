{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRMediaEvent@.
module ObjC.SensorKit.SRMediaEvent
  ( SRMediaEvent
  , IsSRMediaEvent(..)
  , mediaIdentifier
  , eventType
  , mediaIdentifierSelector
  , eventTypeSelector

  -- * Enum types
  , SRMediaEventType(SRMediaEventType)
  , pattern SRMediaEventOnScreen
  , pattern SRMediaEventOffScreen

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

-- | mediaIdentifier
--
-- Unique media identifier
--
-- Unique media identifier to track a specific media object.
--
-- ObjC selector: @- mediaIdentifier@
mediaIdentifier :: IsSRMediaEvent srMediaEvent => srMediaEvent -> IO (Id NSString)
mediaIdentifier srMediaEvent  =
    sendMsg srMediaEvent (mkSelector "mediaIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | eventType
--
-- Type of the event
--
-- Type of media event (e.g., media has been displayed on a screen).
--
-- ObjC selector: @- eventType@
eventType :: IsSRMediaEvent srMediaEvent => srMediaEvent -> IO SRMediaEventType
eventType srMediaEvent  =
    fmap (coerce :: CLong -> SRMediaEventType) $ sendMsg srMediaEvent (mkSelector "eventType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaIdentifier@
mediaIdentifierSelector :: Selector
mediaIdentifierSelector = mkSelector "mediaIdentifier"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector
eventTypeSelector = mkSelector "eventType"

