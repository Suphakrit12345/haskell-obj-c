{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type used to report a volume's statistics.
--
-- The names of this type's properties match those in the @statfs@ structure in @statfs(2)@, which reports these values for an FSKit file system. All numeric properties default to @0@. Override these values, unless a given property has no meaningful value to provide.
--
-- > Note: Available space, free space, total space, and used space have properties to express their values either as a number of blocks or a number of bytes. Your module may supply both of these values by setting both the relevant block or byte property. Alternatively, a module may set only one of the two properties. When you do this, FSKit calculates the matching value based on ``blockSize``.
--
-- For the read-only ``fileSystemTypeName``, set this value with the designated initializer.
--
-- Generated bindings for @FSStatFSResult@.
module ObjC.FSKit.FSStatFSResult
  ( FSStatFSResult
  , IsFSStatFSResult(..)
  , initWithFileSystemTypeName
  , init_
  , blockSize
  , setBlockSize
  , ioSize
  , setIoSize
  , totalBlocks
  , setTotalBlocks
  , availableBlocks
  , setAvailableBlocks
  , freeBlocks
  , setFreeBlocks
  , usedBlocks
  , setUsedBlocks
  , totalBytes
  , setTotalBytes
  , availableBytes
  , setAvailableBytes
  , freeBytes
  , setFreeBytes
  , usedBytes
  , setUsedBytes
  , totalFiles
  , setTotalFiles
  , freeFiles
  , setFreeFiles
  , fileSystemSubType
  , setFileSystemSubType
  , fileSystemTypeName
  , initWithFileSystemTypeNameSelector
  , initSelector
  , blockSizeSelector
  , setBlockSizeSelector
  , ioSizeSelector
  , setIoSizeSelector
  , totalBlocksSelector
  , setTotalBlocksSelector
  , availableBlocksSelector
  , setAvailableBlocksSelector
  , freeBlocksSelector
  , setFreeBlocksSelector
  , usedBlocksSelector
  , setUsedBlocksSelector
  , totalBytesSelector
  , setTotalBytesSelector
  , availableBytesSelector
  , setAvailableBytesSelector
  , freeBytesSelector
  , setFreeBytesSelector
  , usedBytesSelector
  , setUsedBytesSelector
  , totalFilesSelector
  , setTotalFilesSelector
  , freeFilesSelector
  , setFreeFilesSelector
  , fileSystemSubTypeSelector
  , setFileSystemSubTypeSelector
  , fileSystemTypeNameSelector


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

-- | Creates an statistics result instance, using the given file system type name.
--
-- - Parameters fileSystemTypeName: A type name for the file system. The maximum allowed length is @MFSTYPENAMELEN@, including the terminating @NUL@ character.
--
-- ObjC selector: @- initWithFileSystemTypeName:@
initWithFileSystemTypeName :: (IsFSStatFSResult fsStatFSResult, IsNSString fileSystemTypeName) => fsStatFSResult -> fileSystemTypeName -> IO (Id FSStatFSResult)
initWithFileSystemTypeName fsStatFSResult  fileSystemTypeName =
  withObjCPtr fileSystemTypeName $ \raw_fileSystemTypeName ->
      sendMsg fsStatFSResult (mkSelector "initWithFileSystemTypeName:") (retPtr retVoid) [argPtr (castPtr raw_fileSystemTypeName :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO (Id FSStatFSResult)
init_ fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A property for the volume's block size, in bytes.
--
-- This value defaults to @4096@. Zero isn't a valid block size.
--
-- ObjC selector: @- blockSize@
blockSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
blockSize fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "blockSize") retCLong []

-- | A property for the volume's block size, in bytes.
--
-- This value defaults to @4096@. Zero isn't a valid block size.
--
-- ObjC selector: @- setBlockSize:@
setBlockSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setBlockSize fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setBlockSize:") retVoid [argCLong value]

-- | A property for the optimal block size with which to perform I/O.
--
-- For best performance, specify an @ioSize@ that's an even multiple of ``blockSize``.
--
-- ObjC selector: @- ioSize@
ioSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
ioSize fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "ioSize") retCLong []

-- | A property for the optimal block size with which to perform I/O.
--
-- For best performance, specify an @ioSize@ that's an even multiple of ``blockSize``.
--
-- ObjC selector: @- setIoSize:@
setIoSize :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setIoSize fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setIoSize:") retVoid [argCLong value]

-- | A property for the volume's total data block count.
--
-- ObjC selector: @- totalBlocks@
totalBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalBlocks fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "totalBlocks") retCULong []

-- | A property for the volume's total data block count.
--
-- ObjC selector: @- setTotalBlocks:@
setTotalBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalBlocks fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setTotalBlocks:") retVoid [argCULong value]

-- | A property for the number of free blocks available to a non-superuser on the volume.
--
-- ObjC selector: @- availableBlocks@
availableBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
availableBlocks fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "availableBlocks") retCULong []

-- | A property for the number of free blocks available to a non-superuser on the volume.
--
-- ObjC selector: @- setAvailableBlocks:@
setAvailableBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setAvailableBlocks fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setAvailableBlocks:") retVoid [argCULong value]

-- | A property for the number of free blocks in the volume.
--
-- ObjC selector: @- freeBlocks@
freeBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeBlocks fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "freeBlocks") retCULong []

-- | A property for the number of free blocks in the volume.
--
-- ObjC selector: @- setFreeBlocks:@
setFreeBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeBlocks fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setFreeBlocks:") retVoid [argCULong value]

-- | A property for the number of used blocks in the volume.
--
-- ObjC selector: @- usedBlocks@
usedBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
usedBlocks fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "usedBlocks") retCULong []

-- | A property for the number of used blocks in the volume.
--
-- ObjC selector: @- setUsedBlocks:@
setUsedBlocks :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setUsedBlocks fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setUsedBlocks:") retVoid [argCULong value]

-- | A property for the total size, in bytes, of the volume.
--
-- ObjC selector: @- totalBytes@
totalBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalBytes fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "totalBytes") retCULong []

-- | A property for the total size, in bytes, of the volume.
--
-- ObjC selector: @- setTotalBytes:@
setTotalBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalBytes fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setTotalBytes:") retVoid [argCULong value]

-- | A property for the amount of space available to users, in bytes, in the volume.
--
-- ObjC selector: @- availableBytes@
availableBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
availableBytes fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "availableBytes") retCULong []

-- | A property for the amount of space available to users, in bytes, in the volume.
--
-- ObjC selector: @- setAvailableBytes:@
setAvailableBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setAvailableBytes fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setAvailableBytes:") retVoid [argCULong value]

-- | A property for the amount of free space, in bytes, in the volume.
--
-- ObjC selector: @- freeBytes@
freeBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeBytes fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "freeBytes") retCULong []

-- | A property for the amount of free space, in bytes, in the volume.
--
-- ObjC selector: @- setFreeBytes:@
setFreeBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeBytes fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setFreeBytes:") retVoid [argCULong value]

-- | A property for the amount of used space, in bytes, in the volume.
--
-- ObjC selector: @- usedBytes@
usedBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
usedBytes fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "usedBytes") retCULong []

-- | A property for the amount of used space, in bytes, in the volume.
--
-- ObjC selector: @- setUsedBytes:@
setUsedBytes :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setUsedBytes fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setUsedBytes:") retVoid [argCULong value]

-- | A property for the total number of file slots in the volume,
--
-- ObjC selector: @- totalFiles@
totalFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
totalFiles fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "totalFiles") retCULong []

-- | A property for the total number of file slots in the volume,
--
-- ObjC selector: @- setTotalFiles:@
setTotalFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setTotalFiles fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setTotalFiles:") retVoid [argCULong value]

-- | A property for the total number of free file slots in the volume.
--
-- ObjC selector: @- freeFiles@
freeFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CULong
freeFiles fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "freeFiles") retCULong []

-- | A property for the total number of free file slots in the volume.
--
-- ObjC selector: @- setFreeFiles:@
setFreeFiles :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CULong -> IO ()
setFreeFiles fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setFreeFiles:") retVoid [argCULong value]

-- | A property for the file system's subtype or flavor.
--
-- Match this value to the @FSPersonalities@'s @FSSubType@ attribute, if it exists within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@.
--
-- ObjC selector: @- fileSystemSubType@
fileSystemSubType :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO CLong
fileSystemSubType fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "fileSystemSubType") retCLong []

-- | A property for the file system's subtype or flavor.
--
-- Match this value to the @FSPersonalities@'s @FSSubType@ attribute, if it exists within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@.
--
-- ObjC selector: @- setFileSystemSubType:@
setFileSystemSubType :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> CLong -> IO ()
setFileSystemSubType fsStatFSResult  value =
    sendMsg fsStatFSResult (mkSelector "setFileSystemSubType:") retVoid [argCLong value]

-- | A property for the file system type name.
--
-- Match this value to the @FSShortName@ attribute within the @EXAppExtensionAttributes@ dictionary of the module's @Info.plist@. The maximum allowed length is @MFSTYPENAMELEN@, including the terminating @NUL@ character.
--
-- ObjC selector: @- fileSystemTypeName@
fileSystemTypeName :: IsFSStatFSResult fsStatFSResult => fsStatFSResult -> IO (Id NSString)
fileSystemTypeName fsStatFSResult  =
    sendMsg fsStatFSResult (mkSelector "fileSystemTypeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileSystemTypeName:@
initWithFileSystemTypeNameSelector :: Selector
initWithFileSystemTypeNameSelector = mkSelector "initWithFileSystemTypeName:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @blockSize@
blockSizeSelector :: Selector
blockSizeSelector = mkSelector "blockSize"

-- | @Selector@ for @setBlockSize:@
setBlockSizeSelector :: Selector
setBlockSizeSelector = mkSelector "setBlockSize:"

-- | @Selector@ for @ioSize@
ioSizeSelector :: Selector
ioSizeSelector = mkSelector "ioSize"

-- | @Selector@ for @setIoSize:@
setIoSizeSelector :: Selector
setIoSizeSelector = mkSelector "setIoSize:"

-- | @Selector@ for @totalBlocks@
totalBlocksSelector :: Selector
totalBlocksSelector = mkSelector "totalBlocks"

-- | @Selector@ for @setTotalBlocks:@
setTotalBlocksSelector :: Selector
setTotalBlocksSelector = mkSelector "setTotalBlocks:"

-- | @Selector@ for @availableBlocks@
availableBlocksSelector :: Selector
availableBlocksSelector = mkSelector "availableBlocks"

-- | @Selector@ for @setAvailableBlocks:@
setAvailableBlocksSelector :: Selector
setAvailableBlocksSelector = mkSelector "setAvailableBlocks:"

-- | @Selector@ for @freeBlocks@
freeBlocksSelector :: Selector
freeBlocksSelector = mkSelector "freeBlocks"

-- | @Selector@ for @setFreeBlocks:@
setFreeBlocksSelector :: Selector
setFreeBlocksSelector = mkSelector "setFreeBlocks:"

-- | @Selector@ for @usedBlocks@
usedBlocksSelector :: Selector
usedBlocksSelector = mkSelector "usedBlocks"

-- | @Selector@ for @setUsedBlocks:@
setUsedBlocksSelector :: Selector
setUsedBlocksSelector = mkSelector "setUsedBlocks:"

-- | @Selector@ for @totalBytes@
totalBytesSelector :: Selector
totalBytesSelector = mkSelector "totalBytes"

-- | @Selector@ for @setTotalBytes:@
setTotalBytesSelector :: Selector
setTotalBytesSelector = mkSelector "setTotalBytes:"

-- | @Selector@ for @availableBytes@
availableBytesSelector :: Selector
availableBytesSelector = mkSelector "availableBytes"

-- | @Selector@ for @setAvailableBytes:@
setAvailableBytesSelector :: Selector
setAvailableBytesSelector = mkSelector "setAvailableBytes:"

-- | @Selector@ for @freeBytes@
freeBytesSelector :: Selector
freeBytesSelector = mkSelector "freeBytes"

-- | @Selector@ for @setFreeBytes:@
setFreeBytesSelector :: Selector
setFreeBytesSelector = mkSelector "setFreeBytes:"

-- | @Selector@ for @usedBytes@
usedBytesSelector :: Selector
usedBytesSelector = mkSelector "usedBytes"

-- | @Selector@ for @setUsedBytes:@
setUsedBytesSelector :: Selector
setUsedBytesSelector = mkSelector "setUsedBytes:"

-- | @Selector@ for @totalFiles@
totalFilesSelector :: Selector
totalFilesSelector = mkSelector "totalFiles"

-- | @Selector@ for @setTotalFiles:@
setTotalFilesSelector :: Selector
setTotalFilesSelector = mkSelector "setTotalFiles:"

-- | @Selector@ for @freeFiles@
freeFilesSelector :: Selector
freeFilesSelector = mkSelector "freeFiles"

-- | @Selector@ for @setFreeFiles:@
setFreeFilesSelector :: Selector
setFreeFilesSelector = mkSelector "setFreeFiles:"

-- | @Selector@ for @fileSystemSubType@
fileSystemSubTypeSelector :: Selector
fileSystemSubTypeSelector = mkSelector "fileSystemSubType"

-- | @Selector@ for @setFileSystemSubType:@
setFileSystemSubTypeSelector :: Selector
setFileSystemSubTypeSelector = mkSelector "setFileSystemSubType:"

-- | @Selector@ for @fileSystemTypeName@
fileSystemTypeNameSelector :: Selector
fileSystemTypeNameSelector = mkSelector "fileSystemTypeName"

