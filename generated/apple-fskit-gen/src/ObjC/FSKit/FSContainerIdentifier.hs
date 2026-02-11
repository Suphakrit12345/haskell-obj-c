{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that identifies a container.
--
-- The identifier is either a UUID or a UUID with additional differentiating bytes. Some network protocols evaluate access based on a user ID when connecting. In this situation, when a file server receives multiple client connections with different user IDs, the server provides different file hierarchies to each. For such systems, represent the container identifier as the UUID associated with the server, followed by four or eight bytes to differentiate connections.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSContainerIdentifier@.
module ObjC.FSKit.FSContainerIdentifier
  ( FSContainerIdentifier
  , IsFSContainerIdentifier(..)
  , volumeIdentifier
  , volumeIdentifierSelector


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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The volume identifier associated with the container.
--
-- For unary file systems, the volume identifier is the same as the container identifier.
--
-- ObjC selector: @- volumeIdentifier@
volumeIdentifier :: IsFSContainerIdentifier fsContainerIdentifier => fsContainerIdentifier -> IO (Id FSVolumeIdentifier)
volumeIdentifier fsContainerIdentifier  =
    sendMsg fsContainerIdentifier (mkSelector "volumeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @volumeIdentifier@
volumeIdentifierSelector :: Selector
volumeIdentifierSelector = mkSelector "volumeIdentifier"

