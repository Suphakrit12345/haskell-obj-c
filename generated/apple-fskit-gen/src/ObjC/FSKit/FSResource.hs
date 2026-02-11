{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract resource a file system uses to provide data for a volume.
--
-- @FSResource@ is a base class to represent the various possible sources of data for a file system. These range from dedicated storage devices like hard drives and flash storage to network connections, and beyond. Subclasses define behavior specific to a given kind of resource, such as ``FSBlockDeviceResource-c.class`` for disk partition (IOMedia) file systems. These file systems are typical disk file systems such as HFS, APFS, ExFAT, ext2fs, or NTFS.
--
-- A resource's type also determines its life cycle. Resources based on block storage devices come into being when the system probes the media underlying the volumes and container. Other kinds of resources, like those based on URLs, might have different life cycles. For example, a resource based on a @file://@ URL might iniitalize when a person uses the "Connect to server" command in the macOS Finder.
--
-- ### Proxying resources
--
-- Some resources, like ``FSBlockDeviceResource``, come in proxy and non-proxy variants. This addresses the issue that opening an external device like @/dev/disk2s1@ requires an entitlement. Proxy resources allow unentitled clients of FSKit to describe which disk an ``FSBlockDeviceResource`` should represent. This allows, for example, the @mount(8)@ tool to mount FSKit file systems on block devices when run as root. The tool uses a proxy when executing a command like @mount -t ffs /dev/disk2s1 /some/path@, which prevents leaking privileged resource access.
--
-- Generated bindings for @FSResource@.
module ObjC.FSKit.FSResource
  ( FSResource
  , IsFSResource(..)
  , init_
  , makeProxy
  , revoke
  , revoked
  , initSelector
  , makeProxySelector
  , revokeSelector
  , revokedSelector


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

-- | @- init@
init_ :: IsFSResource fsResource => fsResource -> IO (Id FSResource)
init_ fsResource  =
    sendMsg fsResource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a proxy object of this resource.
--
-- If you create a proxy from a proxy resource, this method returns a copy of the proxy.
--
-- ObjC selector: @- makeProxy@
makeProxy :: IsFSResource fsResource => fsResource -> IO (Id FSResource)
makeProxy fsResource  =
    sendMsg fsResource (mkSelector "makeProxy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Revokes the resource.
--
-- This method works by stripping away any underlying privileges associated with the resource. This effectively disconnects this object from its underlying resource.
--
-- ObjC selector: @- revoke@
revoke :: IsFSResource fsResource => fsResource -> IO ()
revoke fsResource  =
    sendMsg fsResource (mkSelector "revoke") retVoid []

-- | A Boolean value that indicates whether the resource is revoked.
--
-- If this is a proxy resource, the value of this property is always @true@ (Swift) or @YES@ (Objective-C).
--
-- ObjC selector: @- revoked@
revoked :: IsFSResource fsResource => fsResource -> IO Bool
revoked fsResource  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsResource (mkSelector "revoked") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @makeProxy@
makeProxySelector :: Selector
makeProxySelector = mkSelector "makeProxy"

-- | @Selector@ for @revoke@
revokeSelector :: Selector
revokeSelector = mkSelector "revoke"

-- | @Selector@ for @revoked@
revokedSelector :: Selector
revokedSelector = mkSelector "revoked"

