{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostPipe
--
-- The IOUSBHostIOSource representing a USB endpoint
--
-- This class provides functionality to transfer data across USB.
--
-- Generated bindings for @IOUSBHostPipe@.
module ObjC.IOUSBHost.IOUSBHostPipe
  ( IOUSBHostPipe
  , IsIOUSBHostPipe(..)
  , adjustPipeWithDescriptors_error
  , setIdleTimeout_error
  , clearStallWithError
  , sendControlRequest_data_bytesTransferred_completionTimeout_error
  , sendControlRequest_data_bytesTransferred_error
  , sendControlRequest_error
  , enqueueControlRequest_data_completionTimeout_error_completionHandler
  , enqueueControlRequest_data_error_completionHandler
  , enqueueControlRequest_error_completionHandler
  , abortWithOption_error
  , abortWithError
  , sendIORequestWithData_bytesTransferred_completionTimeout_error
  , enqueueIORequestWithData_completionTimeout_error_completionHandler
  , sendIORequestWithData_frameList_frameListCount_firstFrameNumber_error
  , enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandler
  , sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error
  , enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandler
  , enableStreamsWithError
  , disableStreamsWithError
  , copyStreamWithStreamID_error
  , originalDescriptors
  , descriptors
  , idleTimeout
  , adjustPipeWithDescriptors_errorSelector
  , setIdleTimeout_errorSelector
  , clearStallWithErrorSelector
  , sendControlRequest_data_bytesTransferred_completionTimeout_errorSelector
  , sendControlRequest_data_bytesTransferred_errorSelector
  , sendControlRequest_errorSelector
  , enqueueControlRequest_data_completionTimeout_error_completionHandlerSelector
  , enqueueControlRequest_data_error_completionHandlerSelector
  , enqueueControlRequest_error_completionHandlerSelector
  , abortWithOption_errorSelector
  , abortWithErrorSelector
  , sendIORequestWithData_bytesTransferred_completionTimeout_errorSelector
  , enqueueIORequestWithData_completionTimeout_error_completionHandlerSelector
  , sendIORequestWithData_frameList_frameListCount_firstFrameNumber_errorSelector
  , enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandlerSelector
  , sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_errorSelector
  , enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandlerSelector
  , enableStreamsWithErrorSelector
  , disableStreamsWithErrorSelector
  , copyStreamWithStreamID_errorSelector
  , originalDescriptorsSelector
  , descriptorsSelector
  , idleTimeoutSelector

  -- * Enum types
  , IOUSBHostAbortOption(IOUSBHostAbortOption)
  , pattern IOUSBHostAbortOptionAsynchronous
  , pattern IOUSBHostAbortOptionSynchronous
  , IOUSBHostIsochronousTransferOptions(IOUSBHostIsochronousTransferOptions)
  , pattern IOUSBHostIsochronousTransferOptionsNone

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
import ObjC.IOUSBHost.Internal.Structs
import ObjC.IOKit.Internal.Structs
import ObjC.IOUSBHost.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Adjust behavior of periodic endpoints to consume a different amount of bus bandwidth
--
-- Periodic (interrupt and isochronous) endpoints reserve bus bandwidth when they are              created, which takes into account max packet size, burst size, and the endpoint              service interval.  If a function driver knows the endpoint will not use all of the              allocated bandwidth, the adjustPolicy method may be used to reduce the              bandwidth reserved for the endpoint.  The original endpoint descriptors should be              copied and modified to adjust max packet size, mult, burst, and interval, and then              passed to adjustPolicy.  The altered descriptors must pass              validateEndpointDescriptor(...) from the kernel for policy changes to be              processed.
--
-- @descriptors@ — Reference to an IOUSBHostIOSourceDescriptors describing the              new endpoint policy
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- adjustPipeWithDescriptors:error:@
adjustPipeWithDescriptors_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> Const (Ptr IOUSBHostIOSourceDescriptors) -> error_ -> IO Bool
adjustPipeWithDescriptors_error iousbHostPipe  descriptors error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "adjustPipeWithDescriptors:error:") retCULong [argPtr (unConst descriptors), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets the desired idle suspend timeout for the interface
--
-- Once the interface is considered idle, it will defer electrical suspend of the              device for the specified duration.
--
-- @idleTimeout@ — The amount of time after all pipes are idle to              wait before suspending the device.
--
-- Returns: YES on success. An IOReturn error code will be reported on failure.
--
-- ObjC selector: @- setIdleTimeout:error:@
setIdleTimeout_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> CDouble -> error_ -> IO Bool
setIdleTimeout_error iousbHostPipe  idleTimeout error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "setIdleTimeout:error:") retCULong [argCDouble idleTimeout, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Clear the halt condition of the pipe.
--
-- When a bulk or interrupt USB endpoint encounters any IO error other than a timeout,              it transitions to a Halted state which must be cleared to perform additional IO on              the endpoint.  This method will clear the halted condition for the endpoint,              including sending a CLEAR_TT_BUFFER control request  (USB 2.0 11.24.2.3) to an              intermediate hub if required.  All pending IO on the endpoint will be aborted, and              the data toggle for the endpoint will also be reset. ClearStall is not required for              control endpoints.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- clearStallWithError:@
clearStallWithError :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> error_ -> IO Bool
clearStallWithError iousbHostPipe  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "clearStallWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on a control endpoint
--
-- This method will send a synchronous request on a control endpoint, and will not              return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @bytesTransferred@ — An NSUInteger reference which will be updated with the byte count              of the completed data phase.
--
-- @completionTimeout@ — Timeout of the request.  If 0, the request will never timeout.              The default value is IOUSBHostDefaultControlCompletionTimeout.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendControlRequest:data:bytesTransferred:completionTimeout:error:@
sendControlRequest_data_bytesTransferred_completionTimeout_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> data_ -> Ptr CULong -> CDouble -> error_ -> IO Bool
sendControlRequest_data_bytesTransferred_completionTimeout_error iousbHostPipe  request data_ bytesTransferred completionTimeout error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendControlRequest:data:bytesTransferred:completionTimeout:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on a control endpoint
--
-- This method will send a synchronous request on a control endpoint, and will not              return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @bytesTransferred@ — An NSUInteger reference which will be updated with the byte count              of the completed data phase.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendControlRequest:data:bytesTransferred:error:@
sendControlRequest_data_bytesTransferred_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> data_ -> Ptr CULong -> error_ -> IO Bool
sendControlRequest_data_bytesTransferred_error iousbHostPipe  request data_ bytesTransferred error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendControlRequest:data:bytesTransferred:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on a control endpoint
--
-- This method will send a synchronous request on a control endpoint, and will not              return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendControlRequest:error:@
sendControlRequest_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> error_ -> IO Bool
sendControlRequest_error iousbHostPipe  request error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendControlRequest:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Enqueue a request on a control endpoint
--
-- This method will enqueue an asynchronous request on a control endpoint.              If successful, the provided completion routine will be called to report the status              of the completed IO. Completions will be serviced in the              IOUSBHostCompletionHandler on the IOUSBHostInterface's dispatch queue.
--
-- @request@ — Reference IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @completionTimeout@ — Timeout of the request.  If 0, the request will              never timeout. The default value is IOUSBHostDefaultControlCompletionTimeout.
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueControlRequest:data:completionTimeout:error:completionHandler:@
enqueueControlRequest_data_completionTimeout_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> data_ -> CDouble -> error_ -> Ptr () -> IO Bool
enqueueControlRequest_data_completionTimeout_error_completionHandler iousbHostPipe  request data_ completionTimeout error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueControlRequest:data:completionTimeout:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Enqueue a request on a control endpoint
--
-- This method will enqueue an asynchronous request on a control endpoint.              If successful, the provided completion routine will be called to report the status              of the completed IO. Completions will be serviced in the              IOUSBHostCompletionHandler on the IOUSBHostInterface's dispatch queue.
--
-- @request@ — Reference IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueControlRequest:data:error:completionHandler:@
enqueueControlRequest_data_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> data_ -> error_ -> Ptr () -> IO Bool
enqueueControlRequest_data_error_completionHandler iousbHostPipe  request data_ error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueControlRequest:data:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Enqueue a request on a control endpoint
--
-- This method will enqueue an asynchronous request on a control endpoint.              If successful, the provided completion routine will be called to report the status              of the completed IO. Completions will be serviced in the              IOUSBHostCompletionHandler on the IOUSBHostInterface's dispatch queue.
--
-- @request@ — Reference IOUSBDeviceRequest structure.
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueControlRequest:error:completionHandler:@
enqueueControlRequest_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> IOUSBDeviceRequest -> error_ -> Ptr () -> IO Bool
enqueueControlRequest_error_completionHandler iousbHostPipe  request error_ completionHandler =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueControlRequest:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Abort pending I/O requests.
--
-- This method will abort all pending I/O requests.  If option includes              IOUSBHostAbortOptionSynchronous, this method will block any new IO              requests unless they are submitted from an aborted IO's completion routine.
--
-- @option@ — IOUSBHostAbortOption by default IOUSBHostAbortOptionSynchronous is used
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortWithOption:error:@
abortWithOption_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> IOUSBHostAbortOption -> error_ -> IO Bool
abortWithOption_error iousbHostPipe  option error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "abortWithOption:error:") retCULong [argCULong (coerce option), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Abort pending I/O requests.
--
-- This method will abort all pending I/O requests.  If option includes              IOUSBHostAbortOptionSynchronous, this method will block any new IO              requests unless they are submitted from an aborted IO's completion routine.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortWithError:@
abortWithError :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> error_ -> IO Bool
abortWithError iousbHostPipe  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "abortWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send an IO request on the source
--
-- This method will send a synchronous request on the IO source, and will not return              until the request is complete.
--
-- @data@ — An NSMutableData* containing the buffer to use for the transfer. nil will send a zero length packet.
--
-- @bytesTransferred@ — NSUInteger pointer which will be updated with the bytes transferred              during the request
--
-- @completionTimeout@ — Timeout of the request.  If 0, the request will never timeout.              Must be 0 for interrupt pipes and streams.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendIORequestWithData:bytesTransferred:completionTimeout:error:@
sendIORequestWithData_bytesTransferred_completionTimeout_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> Ptr CULong -> CDouble -> error_ -> IO Bool
sendIORequestWithData_bytesTransferred_completionTimeout_error iousbHostPipe  data_ bytesTransferred completionTimeout error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendIORequestWithData:bytesTransferred:completionTimeout:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Enqueue an IO request on the source
--
-- This method is used to issue an asynchronous I/O request on a bulk or interrupt              pipe.
--
-- @data@ — An NSMutableData* containing the buffer to use for the transfer. nil will send a zero length packet.
--
-- @completionTimeout@ — Timeout of the request.  If 0, the request will never timeout.              Must be 0 for interrupt pipes and streams.
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueIORequestWithData:completionTimeout:error:completionHandler:@
enqueueIORequestWithData_completionTimeout_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> CDouble -> error_ -> Ptr () -> IO Bool
enqueueIORequestWithData_completionTimeout_error_completionHandler iousbHostPipe  data_ completionTimeout error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueIORequestWithData:completionTimeout:error:completionHandler:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Send a request on an isochronous endpoint
--
-- This method is used to issue isochronous requests.  The caller allocates and              initializes  an array of IOUSBHostIsochronousFrame structures, which is used to              describe the frames that will be transferred.  See
--
-- IOUSBHostIsochronousFrame
--
-- for information regarding structure              initialization requirements and usage.
--
-- @data@ — An NSMutableData* to be used as the backing store for the I/O.
--
-- @frameList@ — Pointer first element in an IOUSBHostIsochronousFrame array.  The array              must contain at least frameListCount elements.
--
-- @frameListCount@ — Number of elements in frameList.
--
-- @firstFrameNumber@ — Frame number which this request should begin on.  The current frame              number can be queried via [IOUSBHostObject getFrameNumber]              If 0, the transfer will start on the next available frame (XHCI only).
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendIORequestWithData:frameList:frameListCount:firstFrameNumber:error:@
sendIORequestWithData_frameList_frameListCount_firstFrameNumber_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> Ptr IOUSBHostIsochronousFrame -> CULong -> CULong -> error_ -> IO Bool
sendIORequestWithData_frameList_frameListCount_firstFrameNumber_error iousbHostPipe  data_ frameList frameListCount firstFrameNumber error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendIORequestWithData:frameList:frameListCount:firstFrameNumber:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr frameList, argCULong frameListCount, argCULong firstFrameNumber, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on an isochronous endpoint
--
-- This method is used to issue isochronous requests.  The caller allocates and              initializes an array of IOUSBHostIsochronousFrame structures, which is used to              describe the frames that will be transferred.  See
--
-- IOUSBHostIsochronousFrame
--
-- for information regarding structure              initialization requirements and usage.
--
-- @data@ — An NSMutableData* to be used as the backing store for the I/O.
--
-- @frameList@ — Pointer first element in an IOUSBHostIsochronousFrame array.  The array              must contain at least frameListCount elements.
--
-- @frameListCount@ — Number of elements in frameList.
--
-- @firstFrameNumber@ — Frame number which this request should begin on.  The current frame              number can be queried via [IOUSBHostObject frameNumberWithTime]              If 0, the transfer will start on the next available frame (XHCI only).
--
-- @completionHandler@ — an IOUSBHostIsochronousCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueIORequestWithData:frameList:frameListCount:firstFrameNumber:error:completionHandler:@
enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> Ptr IOUSBHostIsochronousFrame -> CULong -> CULong -> error_ -> Ptr () -> IO Bool
enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandler iousbHostPipe  data_ frameList frameListCount firstFrameNumber error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueIORequestWithData:frameList:frameListCount:firstFrameNumber:error:completionHandler:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr frameList, argCULong frameListCount, argCULong firstFrameNumber, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Send a request on an isochronous endpoint
--
-- This method is used to issue isochronous requests. The caller allocates and              initializes an array of IOUSBHostIsochronousTransaction structures, which is used to              describe the frames that will be transferred. See
--
-- IOUSBHostIsochronousTransaction
--
-- for information regarding structure              initialization requirements and usage.
--
-- @data@ — An NSMutableData* to be used as the backing store for the I/O.
--
-- @transactionList@ — Pointer to the first element in an IOUSBHostIsochronousTransaction              array.  The array must contain at least transactionListCount elements.
--
-- @transactionListCount@ — Number of elements in transactionList.
--
-- @firstFrameNumber@ — Frame number which this request should begin on.  The current frame              number can be queried via [IOUSBHostObject getFrameNumber]              If 0, the transfer will start on the next available frame (XHCI only).
--
-- @options@ — Flags that specify additional behavior for every transaction in this transfer.              See
--
-- IOUSBHostIsochronousTransferOptions
--
-- for more details.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- sendIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:@
sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> Ptr IOUSBHostIsochronousTransaction -> CULong -> CULong -> IOUSBHostIsochronousTransferOptions -> error_ -> IO Bool
sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error iousbHostPipe  data_ transactionList transactionListCount firstFrameNumber options error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "sendIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr transactionList, argCULong transactionListCount, argCULong firstFrameNumber, argCUInt (coerce options), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on an isochronous endpoint
--
-- This method is used to issue isochronous requests.  The caller allocates and              initializes an array of IOUSBHostIsochronousTransaction structures, which is used to              describe the frames that will be transferred. See
--
-- IOUSBHostIsochronousTransaction
--
-- for information regarding structure              initialization requirements and usage.
--
-- @data@ — An NSMutableData* to be used as the backing store for the I/O.
--
-- @transactionList@ — Pointer to the first element in an IOUSBHostIsochronousTransaction              array.  The array must contain at least transactionListCount elements.
--
-- @transactionListCount@ — Number of elements in transactionList.
--
-- @firstFrameNumber@ — Frame number which this request should begin on.  The current frame              number can be queried via [IOUSBHostObject frameNumberWithTime]              If 0, the transfer will start on the next available frame (XHCI only).
--
-- @options@ — Flags that specify additional behavior for every transaction in this transfer.
--
-- @completionHandler@ — an IOUSBHostIsochronousTransactionCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:completionHandler:@
enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandler :: (IsIOUSBHostPipe iousbHostPipe, IsNSMutableData data_, IsNSError error_) => iousbHostPipe -> data_ -> Ptr IOUSBHostIsochronousTransaction -> CULong -> CULong -> IOUSBHostIsochronousTransferOptions -> error_ -> Ptr () -> IO Bool
enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandler iousbHostPipe  data_ transactionList transactionListCount firstFrameNumber options error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enqueueIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:completionHandler:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr transactionList, argCULong transactionListCount, argCULong firstFrameNumber, argCUInt (coerce options), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Enable streams for the IOUSBHostPipe
--
-- This method changes the operational mode of the IOUSBHostPipe to allow streaming              endpoint transfers, and must be called before copyStream will return any              IOUSBHostStream objects.
--
-- Returns: YES on success, an An IOReturn error will be returned if the pipe, device, or              underlying host controller does not support streams.
--
-- ObjC selector: @- enableStreamsWithError:@
enableStreamsWithError :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> error_ -> IO Bool
enableStreamsWithError iousbHostPipe  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "enableStreamsWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Disable streams for the IOUSBHostPipe
--
-- This method changes the operational mode of the IOUSBHostPipe to disable streaming              endpoint transfers.  Calling this method will synchronously abort any outstanding              calls on existing IOUSBHostStream objects, and therefore all stream contexts should              first be set as non-active on the device via an out-of-band (class-defined)              mechanism (USB 3.1 8.12.1.4).
--
-- Returns: YES on success,  An IOReturn error will be returned if streams were not enabled for              this IOUSBHostPipe.
--
-- ObjC selector: @- disableStreamsWithError:@
disableStreamsWithError :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> error_ -> IO Bool
disableStreamsWithError iousbHostPipe  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostPipe (mkSelector "disableStreamsWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return the stream associated with streamID
--
-- This method will return the stream associated with streamID.              The caller must release the IOUSBHostStream when finished using it.              [IOUSBHostPipe enableStreams] must be called before this              method will return a stream object.
--
-- @streamID@ — Stream ID in the range of 1 to max, where max              can be retrieved by calling getEndpointMaxStreams with              the endpoint descriptors.
--
-- Returns: Pointer to an IOUSBHostStream object or nil. nil may be returned if either the              device or the underlying host controller do not support that stream ID.
--
-- ObjC selector: @- copyStreamWithStreamID:error:@
copyStreamWithStreamID_error :: (IsIOUSBHostPipe iousbHostPipe, IsNSError error_) => iousbHostPipe -> CULong -> error_ -> IO (Id IOUSBHostStream)
copyStreamWithStreamID_error iousbHostPipe  streamID error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostPipe (mkSelector "copyStreamWithStreamID:error:") (retPtr retVoid) [argCULong streamID, argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Retrieve the Original descriptor used when creating the pipe.
--
-- Returns: IOUSBHostIOSourceDescriptors pointer
--
-- ObjC selector: @- originalDescriptors@
originalDescriptors :: IsIOUSBHostPipe iousbHostPipe => iousbHostPipe -> IO (Const (Ptr IOUSBHostIOSourceDescriptors))
originalDescriptors iousbHostPipe  =
    fmap Const $ fmap castPtr $ sendMsg iousbHostPipe (mkSelector "originalDescriptors") (retPtr retVoid) []

-- | Retrieve the current descriptor controlling the endpoint.
--
-- Returns: IOUSBHostIOSourceDescriptors pointer
--
-- ObjC selector: @- descriptors@
descriptors :: IsIOUSBHostPipe iousbHostPipe => iousbHostPipe -> IO (Const (Ptr IOUSBHostIOSourceDescriptors))
descriptors iousbHostPipe  =
    fmap Const $ fmap castPtr $ sendMsg iousbHostPipe (mkSelector "descriptors") (retPtr retVoid) []

-- | Retrieve the current idle suspend timeout.              See
--
-- setIdleTimeout
--
-- Returns: The amount of time after all pipes are idle to wait before              suspending the device,
--
-- ObjC selector: @- idleTimeout@
idleTimeout :: IsIOUSBHostPipe iousbHostPipe => iousbHostPipe -> IO CDouble
idleTimeout iousbHostPipe  =
    sendMsg iousbHostPipe (mkSelector "idleTimeout") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @adjustPipeWithDescriptors:error:@
adjustPipeWithDescriptors_errorSelector :: Selector
adjustPipeWithDescriptors_errorSelector = mkSelector "adjustPipeWithDescriptors:error:"

-- | @Selector@ for @setIdleTimeout:error:@
setIdleTimeout_errorSelector :: Selector
setIdleTimeout_errorSelector = mkSelector "setIdleTimeout:error:"

-- | @Selector@ for @clearStallWithError:@
clearStallWithErrorSelector :: Selector
clearStallWithErrorSelector = mkSelector "clearStallWithError:"

-- | @Selector@ for @sendControlRequest:data:bytesTransferred:completionTimeout:error:@
sendControlRequest_data_bytesTransferred_completionTimeout_errorSelector :: Selector
sendControlRequest_data_bytesTransferred_completionTimeout_errorSelector = mkSelector "sendControlRequest:data:bytesTransferred:completionTimeout:error:"

-- | @Selector@ for @sendControlRequest:data:bytesTransferred:error:@
sendControlRequest_data_bytesTransferred_errorSelector :: Selector
sendControlRequest_data_bytesTransferred_errorSelector = mkSelector "sendControlRequest:data:bytesTransferred:error:"

-- | @Selector@ for @sendControlRequest:error:@
sendControlRequest_errorSelector :: Selector
sendControlRequest_errorSelector = mkSelector "sendControlRequest:error:"

-- | @Selector@ for @enqueueControlRequest:data:completionTimeout:error:completionHandler:@
enqueueControlRequest_data_completionTimeout_error_completionHandlerSelector :: Selector
enqueueControlRequest_data_completionTimeout_error_completionHandlerSelector = mkSelector "enqueueControlRequest:data:completionTimeout:error:completionHandler:"

-- | @Selector@ for @enqueueControlRequest:data:error:completionHandler:@
enqueueControlRequest_data_error_completionHandlerSelector :: Selector
enqueueControlRequest_data_error_completionHandlerSelector = mkSelector "enqueueControlRequest:data:error:completionHandler:"

-- | @Selector@ for @enqueueControlRequest:error:completionHandler:@
enqueueControlRequest_error_completionHandlerSelector :: Selector
enqueueControlRequest_error_completionHandlerSelector = mkSelector "enqueueControlRequest:error:completionHandler:"

-- | @Selector@ for @abortWithOption:error:@
abortWithOption_errorSelector :: Selector
abortWithOption_errorSelector = mkSelector "abortWithOption:error:"

-- | @Selector@ for @abortWithError:@
abortWithErrorSelector :: Selector
abortWithErrorSelector = mkSelector "abortWithError:"

-- | @Selector@ for @sendIORequestWithData:bytesTransferred:completionTimeout:error:@
sendIORequestWithData_bytesTransferred_completionTimeout_errorSelector :: Selector
sendIORequestWithData_bytesTransferred_completionTimeout_errorSelector = mkSelector "sendIORequestWithData:bytesTransferred:completionTimeout:error:"

-- | @Selector@ for @enqueueIORequestWithData:completionTimeout:error:completionHandler:@
enqueueIORequestWithData_completionTimeout_error_completionHandlerSelector :: Selector
enqueueIORequestWithData_completionTimeout_error_completionHandlerSelector = mkSelector "enqueueIORequestWithData:completionTimeout:error:completionHandler:"

-- | @Selector@ for @sendIORequestWithData:frameList:frameListCount:firstFrameNumber:error:@
sendIORequestWithData_frameList_frameListCount_firstFrameNumber_errorSelector :: Selector
sendIORequestWithData_frameList_frameListCount_firstFrameNumber_errorSelector = mkSelector "sendIORequestWithData:frameList:frameListCount:firstFrameNumber:error:"

-- | @Selector@ for @enqueueIORequestWithData:frameList:frameListCount:firstFrameNumber:error:completionHandler:@
enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandlerSelector :: Selector
enqueueIORequestWithData_frameList_frameListCount_firstFrameNumber_error_completionHandlerSelector = mkSelector "enqueueIORequestWithData:frameList:frameListCount:firstFrameNumber:error:completionHandler:"

-- | @Selector@ for @sendIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:@
sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_errorSelector :: Selector
sendIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_errorSelector = mkSelector "sendIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:"

-- | @Selector@ for @enqueueIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:completionHandler:@
enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandlerSelector :: Selector
enqueueIORequestWithData_transactionList_transactionListCount_firstFrameNumber_options_error_completionHandlerSelector = mkSelector "enqueueIORequestWithData:transactionList:transactionListCount:firstFrameNumber:options:error:completionHandler:"

-- | @Selector@ for @enableStreamsWithError:@
enableStreamsWithErrorSelector :: Selector
enableStreamsWithErrorSelector = mkSelector "enableStreamsWithError:"

-- | @Selector@ for @disableStreamsWithError:@
disableStreamsWithErrorSelector :: Selector
disableStreamsWithErrorSelector = mkSelector "disableStreamsWithError:"

-- | @Selector@ for @copyStreamWithStreamID:error:@
copyStreamWithStreamID_errorSelector :: Selector
copyStreamWithStreamID_errorSelector = mkSelector "copyStreamWithStreamID:error:"

-- | @Selector@ for @originalDescriptors@
originalDescriptorsSelector :: Selector
originalDescriptorsSelector = mkSelector "originalDescriptors"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector
descriptorsSelector = mkSelector "descriptors"

-- | @Selector@ for @idleTimeout@
idleTimeoutSelector :: Selector
idleTimeoutSelector = mkSelector "idleTimeout"

