{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that identifies a volume.
--
-- For most volumes, the volume identifier is the UUID identifying the volume.
--
-- Network file systems may access the same underlying volume using different authentication credentials. To handle this situation, add qualifying data to identify the specific container, as discussed in the superclass, ``FSEntityIdentifier``.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSVolumeIdentifier@.
module ObjC.FSKit.FSVolumeIdentifier
  ( FSVolumeIdentifier
  , IsFSVolumeIdentifier(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

