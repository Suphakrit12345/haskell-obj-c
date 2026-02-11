{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOUSBHostStream@.
module ObjC.IOUSBHost.IOUSBHostStream
  ( IOUSBHostStream
  , IsIOUSBHostStream(..)
  , abortWithOption_error
  , abortWithError
  , sendIORequestWithData_bytesTransferred_error
  , enqueueIORequestWithData_error_completionHandler
  , hostPipe
  , streamID
  , abortWithOption_errorSelector
  , abortWithErrorSelector
  , sendIORequestWithData_bytesTransferred_errorSelector
  , enqueueIORequestWithData_error_completionHandlerSelector
  , hostPipeSelector
  , streamIDSelector

  -- * Enum types
  , IOUSBHostAbortOption(IOUSBHostAbortOption)
  , pattern IOUSBHostAbortOptionAsynchronous
  , pattern IOUSBHostAbortOptionSynchronous

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

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOUSBHost.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Abort pending I/O requests.
--
-- A stream context must be set as non-active on the device via an out-of-band              (class-defined) mechanism before this method is called (USB 3.1 8.12.1.4).              A non-active stream will not be selected by the device to become the current              stream on the endpoint.
--
-- @option@ — IOUSBHostAbortOption by default IOUSBHostAbortOptionSynchronous is used
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortWithOption:error:@
abortWithOption_error :: (IsIOUSBHostStream iousbHostStream, IsNSError error_) => iousbHostStream -> IOUSBHostAbortOption -> error_ -> IO Bool
abortWithOption_error iousbHostStream  option error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostStream (mkSelector "abortWithOption:error:") retCULong [argCULong (coerce option), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Abort pending I/O requests.
--
-- A stream context must be set as non-active on the device via an out-of-band              (class-defined) mechanism before this method is called (USB 3.1 8.12.1.4).              A non-active stream will not be selected by the device to become the current              stream on the endpoint.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortWithError:@
abortWithError :: (IsIOUSBHostStream iousbHostStream, IsNSError error_) => iousbHostStream -> error_ -> IO Bool
abortWithError iousbHostStream  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostStream (mkSelector "abortWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send an IO request on the source
--
-- This method will send a synchronous request on the IO source, and will not return              until the request is complete. CompletionTimeouts are not applicable to streams.
--
-- @data@ — NSData* pointer containing the buffer to use for the transfer
--
-- @bytesTransferred@ — NSUInteger reference which will be updated with the bytes              transferred during the request
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendIORequestWithData:bytesTransferred:error:@
sendIORequestWithData_bytesTransferred_error :: (IsIOUSBHostStream iousbHostStream, IsNSMutableData data_, IsNSError error_) => iousbHostStream -> data_ -> Ptr CULong -> error_ -> IO Bool
sendIORequestWithData_bytesTransferred_error iousbHostStream  data_ bytesTransferred error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostStream (mkSelector "sendIORequestWithData:bytesTransferred:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Enqueue an IO request on the source
--
-- This method is used to issue an asynchronous I/O request on the IO source.              CompletionTimeouts are not applicable to streams.
--
-- @data@ — pointer containing the buffer to use for the transfer
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueIORequestWithData:error:completionHandler:@
enqueueIORequestWithData_error_completionHandler :: (IsIOUSBHostStream iousbHostStream, IsNSMutableData data_, IsNSError error_) => iousbHostStream -> data_ -> error_ -> Ptr () -> IO Bool
enqueueIORequestWithData_error_completionHandler iousbHostStream  data_ error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostStream (mkSelector "enqueueIORequestWithData:error:completionHandler:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Returns the IOUSBHostPipe this stream was created from
--
-- Returns: IOUSBHostPipe pointer
--
-- ObjC selector: @- hostPipe@
hostPipe :: IsIOUSBHostStream iousbHostStream => iousbHostStream -> IO (Id IOUSBHostPipe)
hostPipe iousbHostStream  =
    sendMsg iousbHostStream (mkSelector "hostPipe") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns streamID associated with this IOUSBHostStream.
--
-- Returns: streamID
--
-- ObjC selector: @- streamID@
streamID :: IsIOUSBHostStream iousbHostStream => iousbHostStream -> IO CULong
streamID iousbHostStream  =
    sendMsg iousbHostStream (mkSelector "streamID") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @abortWithOption:error:@
abortWithOption_errorSelector :: Selector
abortWithOption_errorSelector = mkSelector "abortWithOption:error:"

-- | @Selector@ for @abortWithError:@
abortWithErrorSelector :: Selector
abortWithErrorSelector = mkSelector "abortWithError:"

-- | @Selector@ for @sendIORequestWithData:bytesTransferred:error:@
sendIORequestWithData_bytesTransferred_errorSelector :: Selector
sendIORequestWithData_bytesTransferred_errorSelector = mkSelector "sendIORequestWithData:bytesTransferred:error:"

-- | @Selector@ for @enqueueIORequestWithData:error:completionHandler:@
enqueueIORequestWithData_error_completionHandlerSelector :: Selector
enqueueIORequestWithData_error_completionHandlerSelector = mkSelector "enqueueIORequestWithData:error:completionHandler:"

-- | @Selector@ for @hostPipe@
hostPipeSelector :: Selector
hostPipeSelector = mkSelector "hostPipe"

-- | @Selector@ for @streamID@
streamIDSelector :: Selector
streamIDSelector = mkSelector "streamID"

