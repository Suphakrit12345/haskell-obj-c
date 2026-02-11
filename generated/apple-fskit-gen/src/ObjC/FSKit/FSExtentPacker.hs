{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that directs the kernel to map space on disk to a specific file managed by this file system.
--
-- _Extents_ provide the kernel the logical-to-physical mapping of a given file. An extent describes a physical offset on disk, and a length and a logical offset within the file. Rather than working with extents directly, you use this type's methods to provide or "pack" extent information, which FSKit then passes to the kernel.
--
-- Generated bindings for @FSExtentPacker@.
module ObjC.FSKit.FSExtentPacker
  ( FSExtentPacker
  , IsFSExtentPacker(..)
  , init_
  , packExtentWithResource_type_logicalOffset_physicalOffset_length
  , initSelector
  , packExtentWithResource_type_logicalOffset_physicalOffset_lengthSelector

  -- * Enum types
  , FSExtentType(FSExtentType)
  , pattern FSExtentTypeData
  , pattern FSExtentTypeZeroFill

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
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSExtentPacker fsExtentPacker => fsExtentPacker -> IO (Id FSExtentPacker)
init_ fsExtentPacker  =
    sendMsg fsExtentPacker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Packs a single extent to send to the kernel.
--
-- - Parameters:   - resource: The resource on which to perform I/O.   - type: The type of extent, indicating whether it contains valid data.   - logicalOffset: The extent offset within the file, in bytes.   - physicalOffset: The extent offset on disk, in bytes.   - length: The extent length, in bytes. - Returns: A Boolean value that indicates whether the packer can pack more extents.
--
-- ObjC selector: @- packExtentWithResource:type:logicalOffset:physicalOffset:length:@
packExtentWithResource_type_logicalOffset_physicalOffset_length :: (IsFSExtentPacker fsExtentPacker, IsFSBlockDeviceResource resource) => fsExtentPacker -> resource -> FSExtentType -> CLong -> CLong -> CULong -> IO Bool
packExtentWithResource_type_logicalOffset_physicalOffset_length fsExtentPacker  resource type_ logicalOffset physicalOffset length_ =
  withObjCPtr resource $ \raw_resource ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsExtentPacker (mkSelector "packExtentWithResource:type:logicalOffset:physicalOffset:length:") retCULong [argPtr (castPtr raw_resource :: Ptr ()), argCLong (coerce type_), argCLong logicalOffset, argCLong physicalOffset, argCULong length_]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @packExtentWithResource:type:logicalOffset:physicalOffset:length:@
packExtentWithResource_type_logicalOffset_physicalOffset_lengthSelector :: Selector
packExtentWithResource_type_logicalOffset_physicalOffset_lengthSelector = mkSelector "packExtentWithResource:type:logicalOffset:physicalOffset:length:"

