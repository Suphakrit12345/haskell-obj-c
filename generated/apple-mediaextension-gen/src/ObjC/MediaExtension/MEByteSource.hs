{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEByteSource
--
-- Provides read access to the data in a media asset file.
--
-- The Media Toolbox passes an MEByteSource instance for the media asset's primary file when initializing an MEFormatReader object. The MEFormatReader may request additional MEByteSources be created for related files in the same directory as the primary file by calling the byteSourceForRelatedFileName method.
--
-- Generated bindings for @MEByteSource@.
module ObjC.MediaExtension.MEByteSource
  ( MEByteSource
  , IsMEByteSource(..)
  , new
  , init_
  , readDataOfLength_fromOffset_toDestination_completionHandler
  , readDataOfLength_fromOffset_completionHandler
  , availableLengthAtOffset
  , byteSourceForRelatedFileName_error
  , fileName
  , fileLength
  , relatedFileNamesInSameDirectory
  , newSelector
  , initSelector
  , readDataOfLength_fromOffset_toDestination_completionHandlerSelector
  , readDataOfLength_fromOffset_completionHandlerSelector
  , availableLengthAtOffsetSelector
  , byteSourceForRelatedFileName_errorSelector
  , fileNameSelector
  , fileLengthSelector
  , relatedFileNamesInSameDirectorySelector


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

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEByteSource)
new  =
  do
    cls' <- getRequiredClass "MEByteSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEByteSource meByteSource => meByteSource -> IO (Id MEByteSource)
init_ meByteSource  =
    sendMsg meByteSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | readDataOfLength:fromOffset:toDestination:completionHandler:
--
-- Reads bytes from an MEByteSource asynchronously into a buffer.
--
-- Asynchronously reads out the specified number of bytes starting at the indicated offset. Returns the actual number of bytes read out in bytesRead. Read attempts that extend beyond the end of the MEByteSource will succeed if they include at least one valid byte before the end of the MEByteSource.
--
-- @length@ — The number of bytes to read.
--
-- @offset@ — The relative offset in bytes from the beginning of the file from which to start reading.
--
-- @dest@ — The block of memory to hold the data to be read.  Must be at least num bytes in length.
--
-- @completionHandler@ — The handler that will be invoked when the method completes.		'bytesRead'			The actual number of bytes read.		'error'			An NSError object that will contain error information if the method fails, otherwise nil. Returns MEErrorEndOfStream if no more bytes can be read.
--
-- ObjC selector: @- readDataOfLength:fromOffset:toDestination:completionHandler:@
readDataOfLength_fromOffset_toDestination_completionHandler :: IsMEByteSource meByteSource => meByteSource -> CULong -> CLong -> Ptr () -> Ptr () -> IO ()
readDataOfLength_fromOffset_toDestination_completionHandler meByteSource  length_ offset dest completionHandler =
    sendMsg meByteSource (mkSelector "readDataOfLength:fromOffset:toDestination:completionHandler:") retVoid [argCULong length_, argCLong offset, argPtr dest, argPtr (castPtr completionHandler :: Ptr ())]

-- | readDataOfLength:fromOffset:completionHandler:
--
-- Reads bytes from an MEByteSource asynchronously into an NSData object.
--
-- Asynchronously reads out the specified number of bytes starting at the indicated offset. Returns the actual number of bytes read out in bytesRead. Read attempts that extend beyond the end of the MEByteSource will succeed if they include at least one valid byte before the end of the MEByteSource.
--
-- @length@ — The number of bytes to read.
--
-- @offset@ — The relative offset in bytes from the beginning of the file from which to start reading.
--
-- @completionHandler@ — Completion block called when the method completes.		'data'			The NSData object holding the data that have been read. The NSData length property will indicate the actual number of bytes read.		'error'			An NSError object that will contain error information if the method fails, otherwise nil. Returns MEErrorEndOfStream if no more bytes can be read.
--
-- ObjC selector: @- readDataOfLength:fromOffset:completionHandler:@
readDataOfLength_fromOffset_completionHandler :: IsMEByteSource meByteSource => meByteSource -> CULong -> CLong -> Ptr () -> IO ()
readDataOfLength_fromOffset_completionHandler meByteSource  length_ offset completionHandler =
    sendMsg meByteSource (mkSelector "readDataOfLength:fromOffset:completionHandler:") retVoid [argCULong length_, argCLong offset, argPtr (castPtr completionHandler :: Ptr ())]

-- | availableLengthAtOffset:
--
-- Returns the number of available bytes from the offset within the MEByteSource.
--
-- Returns the number of available bytes at the time of the query. This value could change over time. Attempting to read past this value may cause slow I/O.
--
-- @offset@ — The offset in bytes from the beginning of the MEByteSource.
--
-- Returns: Returns the number of available bytes from the offset, or 0 if that information is not available.
--
-- ObjC selector: @- availableLengthAtOffset:@
availableLengthAtOffset :: IsMEByteSource meByteSource => meByteSource -> CLong -> IO CLong
availableLengthAtOffset meByteSource  offset =
    sendMsg meByteSource (mkSelector "availableLengthAtOffset:") retCLong [argCLong offset]

-- | byteSourceForRelatedFileName:error:
--
-- Requests creation of a new MEByteSource for a related file.
--
-- Requests creation of a new MEByteSource for a file related to the receiving MEByteSource. The scope of fileName that may be opened is restricted. Only files in the same directory as the receiver MEByteSource may be accessed, and the file extension must match one of the extensions listed in the format reader bundle plist.
--
-- @fileName@ — The relative file name in the receiver MEByteSource's parent directory.
--
-- @errorOut@ — Reports any errors. Returns MEErrorPermissionDenied if the file cannot be accessed or is prohibited.
--
-- Returns: Returns nil if fileName refers to a file that cannot be accessed or is prohibited, or if an error occured. The returned MEByteSource is autoreleased.
--
-- ObjC selector: @- byteSourceForRelatedFileName:error:@
byteSourceForRelatedFileName_error :: (IsMEByteSource meByteSource, IsNSString fileName, IsNSError errorOut) => meByteSource -> fileName -> errorOut -> IO (Id MEByteSource)
byteSourceForRelatedFileName_error meByteSource  fileName errorOut =
  withObjCPtr fileName $ \raw_fileName ->
    withObjCPtr errorOut $ \raw_errorOut ->
        sendMsg meByteSource (mkSelector "byteSourceForRelatedFileName:error:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_errorOut :: Ptr ())] >>= retainedObject . castPtr

-- | fileName
--
-- The name of a MEByteSource's file.
--
-- The name of the source file for the MEByteSource.
--
-- ObjC selector: @- fileName@
fileName :: IsMEByteSource meByteSource => meByteSource -> IO (Id NSString)
fileName meByteSource  =
    sendMsg meByteSource (mkSelector "fileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileLength
--
-- The length of the MEByteSource's file.
--
-- The length in bytes of the source file for the MEByteSource, or 0 if that information is not available.
--
-- ObjC selector: @- fileLength@
fileLength :: IsMEByteSource meByteSource => meByteSource -> IO CLong
fileLength meByteSource  =
    sendMsg meByteSource (mkSelector "fileLength") retCLong []

-- | relatedFileNamesInSameDirectory
--
-- The array of related file names in the MEByteSource's parent directory.
--
-- The array of related files within the MEByteSource's parent directory that are accessible to the MEByteSource. Only the relative file names are returned, not the paths. If no related files are available, returns an empty array.
--
-- ObjC selector: @- relatedFileNamesInSameDirectory@
relatedFileNamesInSameDirectory :: IsMEByteSource meByteSource => meByteSource -> IO (Id NSArray)
relatedFileNamesInSameDirectory meByteSource  =
    sendMsg meByteSource (mkSelector "relatedFileNamesInSameDirectory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @readDataOfLength:fromOffset:toDestination:completionHandler:@
readDataOfLength_fromOffset_toDestination_completionHandlerSelector :: Selector
readDataOfLength_fromOffset_toDestination_completionHandlerSelector = mkSelector "readDataOfLength:fromOffset:toDestination:completionHandler:"

-- | @Selector@ for @readDataOfLength:fromOffset:completionHandler:@
readDataOfLength_fromOffset_completionHandlerSelector :: Selector
readDataOfLength_fromOffset_completionHandlerSelector = mkSelector "readDataOfLength:fromOffset:completionHandler:"

-- | @Selector@ for @availableLengthAtOffset:@
availableLengthAtOffsetSelector :: Selector
availableLengthAtOffsetSelector = mkSelector "availableLengthAtOffset:"

-- | @Selector@ for @byteSourceForRelatedFileName:error:@
byteSourceForRelatedFileName_errorSelector :: Selector
byteSourceForRelatedFileName_errorSelector = mkSelector "byteSourceForRelatedFileName:error:"

-- | @Selector@ for @fileName@
fileNameSelector :: Selector
fileNameSelector = mkSelector "fileName"

-- | @Selector@ for @fileLength@
fileLengthSelector :: Selector
fileLengthSelector = mkSelector "fileLength"

-- | @Selector@ for @relatedFileNamesInSameDirectory@
relatedFileNamesInSameDirectorySelector :: Selector
relatedFileNamesInSameDirectorySelector = mkSelector "relatedFileNamesInSameDirectory"

