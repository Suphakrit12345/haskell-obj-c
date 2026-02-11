{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Attributes of an item, such as size, creation and modification times, and user and group identifiers.
--
-- Generated bindings for @FSItemAttributes@.
module ObjC.FSKit.FSItemAttributes
  ( FSItemAttributes
  , IsFSItemAttributes(..)
  , invalidateAllProperties
  , isValid
  , uid
  , setUid
  , gid
  , setGid
  , mode
  , setMode
  , type_
  , setType
  , linkCount
  , setLinkCount
  , flags
  , setFlags
  , size
  , setSize
  , allocSize
  , setAllocSize
  , fileID
  , setFileID
  , parentID
  , setParentID
  , supportsLimitedXAttrs
  , setSupportsLimitedXAttrs
  , inhibitKernelOffloadedIO
  , setInhibitKernelOffloadedIO
  , invalidateAllPropertiesSelector
  , isValidSelector
  , uidSelector
  , setUidSelector
  , gidSelector
  , setGidSelector
  , modeSelector
  , setModeSelector
  , typeSelector
  , setTypeSelector
  , linkCountSelector
  , setLinkCountSelector
  , flagsSelector
  , setFlagsSelector
  , sizeSelector
  , setSizeSelector
  , allocSizeSelector
  , setAllocSizeSelector
  , fileIDSelector
  , setFileIDSelector
  , parentIDSelector
  , setParentIDSelector
  , supportsLimitedXAttrsSelector
  , setSupportsLimitedXAttrsSelector
  , inhibitKernelOffloadedIOSelector
  , setInhibitKernelOffloadedIOSelector

  -- * Enum types
  , FSItemAttribute(FSItemAttribute)
  , pattern FSItemAttributeType
  , pattern FSItemAttributeMode
  , pattern FSItemAttributeLinkCount
  , pattern FSItemAttributeUID
  , pattern FSItemAttributeGID
  , pattern FSItemAttributeFlags
  , pattern FSItemAttributeSize
  , pattern FSItemAttributeAllocSize
  , pattern FSItemAttributeFileID
  , pattern FSItemAttributeParentID
  , pattern FSItemAttributeAccessTime
  , pattern FSItemAttributeModifyTime
  , pattern FSItemAttributeChangeTime
  , pattern FSItemAttributeBirthTime
  , pattern FSItemAttributeBackupTime
  , pattern FSItemAttributeAddedTime
  , pattern FSItemAttributeSupportsLimitedXAttrs
  , pattern FSItemAttributeInhibitKernelOffloadedIO
  , FSItemID(FSItemID)
  , pattern FSItemIDInvalid
  , pattern FSItemIDParentOfRoot
  , pattern FSItemIDRootDirectory
  , FSItemType(FSItemType)
  , pattern FSItemTypeUnknown
  , pattern FSItemTypeFile
  , pattern FSItemTypeDirectory
  , pattern FSItemTypeSymlink
  , pattern FSItemTypeFIFO
  , pattern FSItemTypeCharDevice
  , pattern FSItemTypeBlockDevice
  , pattern FSItemTypeSocket

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Marks all attributes inactive.
--
-- ObjC selector: @- invalidateAllProperties@
invalidateAllProperties :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO ()
invalidateAllProperties fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "invalidateAllProperties") retVoid []

-- | Returns a Boolean value that indicates whether the attribute is valid.
--
-- If the value returned by this method is @YES@ (Objective-C) or @true@ (Swift), a caller can safely use the given attribute.
--
-- ObjC selector: @- isValid:@
isValid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemAttribute -> IO Bool
isValid fsItemAttributes  attribute =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsItemAttributes (mkSelector "isValid:") retCULong [argCLong (coerce attribute)]

-- | The user identifier.
--
-- ObjC selector: @- uid@
uid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
uid fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "uid") retCUInt []

-- | The user identifier.
--
-- ObjC selector: @- setUid:@
setUid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setUid fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setUid:") retVoid [argCUInt value]

-- | The group identifier.
--
-- ObjC selector: @- gid@
gid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
gid fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "gid") retCUInt []

-- | The group identifier.
--
-- ObjC selector: @- setGid:@
setGid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setGid fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setGid:") retVoid [argCUInt value]

-- | The mode of the item.
--
-- The mode is often used for @setuid@, @setgid@, and @sticky@ bits.
--
-- ObjC selector: @- mode@
mode :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
mode fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "mode") retCUInt []

-- | The mode of the item.
--
-- The mode is often used for @setuid@, @setgid@, and @sticky@ bits.
--
-- ObjC selector: @- setMode:@
setMode :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setMode fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setMode:") retVoid [argCUInt value]

-- | The item type, such as a regular file, directory, or symbolic link.
--
-- ObjC selector: @- type@
type_ :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemType
type_ fsItemAttributes  =
    fmap (coerce :: CLong -> FSItemType) $ sendMsg fsItemAttributes (mkSelector "type") retCLong []

-- | The item type, such as a regular file, directory, or symbolic link.
--
-- ObjC selector: @- setType:@
setType :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemType -> IO ()
setType fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | The number of hard links to the item.
--
-- ObjC selector: @- linkCount@
linkCount :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
linkCount fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "linkCount") retCUInt []

-- | The number of hard links to the item.
--
-- ObjC selector: @- setLinkCount:@
setLinkCount :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setLinkCount fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setLinkCount:") retVoid [argCUInt value]

-- | The item's behavior flags.
--
-- See @st_flags@ in @stat.h@ for flag definitions.
--
-- ObjC selector: @- flags@
flags :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
flags fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "flags") retCUInt []

-- | The item's behavior flags.
--
-- See @st_flags@ in @stat.h@ for flag definitions.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setFlags fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setFlags:") retVoid [argCUInt value]

-- | The item's size.
--
-- ObjC selector: @- size@
size :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CULong
size fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "size") retCULong []

-- | The item's size.
--
-- ObjC selector: @- setSize:@
setSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CULong -> IO ()
setSize fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setSize:") retVoid [argCULong value]

-- | The item's allocated size.
--
-- ObjC selector: @- allocSize@
allocSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CULong
allocSize fsItemAttributes  =
    sendMsg fsItemAttributes (mkSelector "allocSize") retCULong []

-- | The item's allocated size.
--
-- ObjC selector: @- setAllocSize:@
setAllocSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CULong -> IO ()
setAllocSize fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setAllocSize:") retVoid [argCULong value]

-- | The item's file identifier.
--
-- ObjC selector: @- fileID@
fileID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemID
fileID fsItemAttributes  =
    fmap (coerce :: CULong -> FSItemID) $ sendMsg fsItemAttributes (mkSelector "fileID") retCULong []

-- | The item's file identifier.
--
-- ObjC selector: @- setFileID:@
setFileID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemID -> IO ()
setFileID fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setFileID:") retVoid [argCULong (coerce value)]

-- | The identifier of the item's parent.
--
-- ObjC selector: @- parentID@
parentID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemID
parentID fsItemAttributes  =
    fmap (coerce :: CULong -> FSItemID) $ sendMsg fsItemAttributes (mkSelector "parentID") retCULong []

-- | The identifier of the item's parent.
--
-- ObjC selector: @- setParentID:@
setParentID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemID -> IO ()
setParentID fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setParentID:") retVoid [argCULong (coerce value)]

-- | A Boolean value that indicates whether the item supports a limited set of extended attributes.
--
-- ObjC selector: @- supportsLimitedXAttrs@
supportsLimitedXAttrs :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO Bool
supportsLimitedXAttrs fsItemAttributes  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsItemAttributes (mkSelector "supportsLimitedXAttrs") retCULong []

-- | A Boolean value that indicates whether the item supports a limited set of extended attributes.
--
-- ObjC selector: @- setSupportsLimitedXAttrs:@
setSupportsLimitedXAttrs :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> Bool -> IO ()
setSupportsLimitedXAttrs fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setSupportsLimitedXAttrs:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether the file system overrides the per-volume settings for kernel offloaded I/O for a specific file.
--
-- This property has no meaning if the volume doesn't conform to ``FSVolumeKernelOffloadedIOOperations``.
--
-- ObjC selector: @- inhibitKernelOffloadedIO@
inhibitKernelOffloadedIO :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO Bool
inhibitKernelOffloadedIO fsItemAttributes  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsItemAttributes (mkSelector "inhibitKernelOffloadedIO") retCULong []

-- | A Boolean value that indicates whether the file system overrides the per-volume settings for kernel offloaded I/O for a specific file.
--
-- This property has no meaning if the volume doesn't conform to ``FSVolumeKernelOffloadedIOOperations``.
--
-- ObjC selector: @- setInhibitKernelOffloadedIO:@
setInhibitKernelOffloadedIO :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> Bool -> IO ()
setInhibitKernelOffloadedIO fsItemAttributes  value =
    sendMsg fsItemAttributes (mkSelector "setInhibitKernelOffloadedIO:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateAllProperties@
invalidateAllPropertiesSelector :: Selector
invalidateAllPropertiesSelector = mkSelector "invalidateAllProperties"

-- | @Selector@ for @isValid:@
isValidSelector :: Selector
isValidSelector = mkSelector "isValid:"

-- | @Selector@ for @uid@
uidSelector :: Selector
uidSelector = mkSelector "uid"

-- | @Selector@ for @setUid:@
setUidSelector :: Selector
setUidSelector = mkSelector "setUid:"

-- | @Selector@ for @gid@
gidSelector :: Selector
gidSelector = mkSelector "gid"

-- | @Selector@ for @setGid:@
setGidSelector :: Selector
setGidSelector = mkSelector "setGid:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @linkCount@
linkCountSelector :: Selector
linkCountSelector = mkSelector "linkCount"

-- | @Selector@ for @setLinkCount:@
setLinkCountSelector :: Selector
setLinkCountSelector = mkSelector "setLinkCount:"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @allocSize@
allocSizeSelector :: Selector
allocSizeSelector = mkSelector "allocSize"

-- | @Selector@ for @setAllocSize:@
setAllocSizeSelector :: Selector
setAllocSizeSelector = mkSelector "setAllocSize:"

-- | @Selector@ for @fileID@
fileIDSelector :: Selector
fileIDSelector = mkSelector "fileID"

-- | @Selector@ for @setFileID:@
setFileIDSelector :: Selector
setFileIDSelector = mkSelector "setFileID:"

-- | @Selector@ for @parentID@
parentIDSelector :: Selector
parentIDSelector = mkSelector "parentID"

-- | @Selector@ for @setParentID:@
setParentIDSelector :: Selector
setParentIDSelector = mkSelector "setParentID:"

-- | @Selector@ for @supportsLimitedXAttrs@
supportsLimitedXAttrsSelector :: Selector
supportsLimitedXAttrsSelector = mkSelector "supportsLimitedXAttrs"

-- | @Selector@ for @setSupportsLimitedXAttrs:@
setSupportsLimitedXAttrsSelector :: Selector
setSupportsLimitedXAttrsSelector = mkSelector "setSupportsLimitedXAttrs:"

-- | @Selector@ for @inhibitKernelOffloadedIO@
inhibitKernelOffloadedIOSelector :: Selector
inhibitKernelOffloadedIOSelector = mkSelector "inhibitKernelOffloadedIO"

-- | @Selector@ for @setInhibitKernelOffloadedIO:@
setInhibitKernelOffloadedIOSelector :: Selector
setInhibitKernelOffloadedIOSelector = mkSelector "setInhibitKernelOffloadedIO:"

