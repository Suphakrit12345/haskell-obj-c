{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterRequestCommissioningApprovalParams@.
module ObjC.Matter.MTRCommissionerControlClusterRequestCommissioningApprovalParams
  ( MTRCommissionerControlClusterRequestCommissioningApprovalParams
  , IsMTRCommissionerControlClusterRequestCommissioningApprovalParams(..)
  , requestID
  , setRequestID
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , label
  , setLabel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , requestIDSelector
  , setRequestIDSelector
  , vendorIDSelector
  , setVendorIDSelector
  , productIDSelector
  , setProductIDSelector
  , labelSelector
  , setLabelSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- requestID@
requestID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
requestID mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "requestID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestID:@
setRequestID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setRequestID mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setRequestID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorID@
vendorID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
vendorID mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setVendorID mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productID@
productID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
productID mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductID:@
setProductID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setProductID mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setProductID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSString)
label mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSString value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setLabel mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCommissionerControlClusterRequestCommissioningApprovalParams  =
    sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setServerSideProcessingTimeout mtrCommissionerControlClusterRequestCommissioningApprovalParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterRequestCommissioningApprovalParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestID@
requestIDSelector :: Selector
requestIDSelector = mkSelector "requestID"

-- | @Selector@ for @setRequestID:@
setRequestIDSelector :: Selector
setRequestIDSelector = mkSelector "setRequestID:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector
setProductIDSelector = mkSelector "setProductID:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

