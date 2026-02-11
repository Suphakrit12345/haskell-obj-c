{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for implementing a full-featured file system.
--
-- @FSFileSystem@ is a full-featured file system, which works with one or more ``FSResource`` instances and presents one or more ``FSVolume`` references to callers.
--
-- Implement your app extension by providing a subclass of @FSFileSystem@ as a delegate object. Your delegate also needs to implement the @FSFileSystemOperations@ protocol so that it can probe, load, and unload resources.
--
-- > Note: The current version of FSKit supports only ``FSUnaryFileSystem``, not @FSFileSystem@.
--
-- Generated bindings for @FSFileSystem@.
module ObjC.FSKit.FSFileSystem
  ( FSFileSystem
  , IsFSFileSystem(..)


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

