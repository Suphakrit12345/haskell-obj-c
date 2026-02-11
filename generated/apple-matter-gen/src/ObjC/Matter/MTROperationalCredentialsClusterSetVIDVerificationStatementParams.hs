{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSetVIDVerificationStatementParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSetVIDVerificationStatementParams
  ( MTROperationalCredentialsClusterSetVIDVerificationStatementParams
  , IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams(..)
  , vendorID
  , setVendorID
  , vidVerificationStatement
  , setVidVerificationStatement
  , vvsc
  , setVvsc
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , vendorIDSelector
  , setVendorIDSelector
  , vidVerificationStatementSelector
  , setVidVerificationStatementSelector
  , vvscSelector
  , setVvscSelector
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

-- | @- vendorID@
vendorID :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
vendorID mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  =
    sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVendorID mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vidVerificationStatement@
vidVerificationStatement :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSData)
vidVerificationStatement mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  =
    sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "vidVerificationStatement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVidVerificationStatement:@
setVidVerificationStatement :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSData value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVidVerificationStatement mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "setVidVerificationStatement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vvsc@
vvsc :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSData)
vvsc mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  =
    sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "vvsc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVvsc:@
setVvsc :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSData value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVvsc mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "setVvsc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  =
    sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  =
    sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterSetVIDVerificationStatementParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSetVIDVerificationStatementParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @vidVerificationStatement@
vidVerificationStatementSelector :: Selector
vidVerificationStatementSelector = mkSelector "vidVerificationStatement"

-- | @Selector@ for @setVidVerificationStatement:@
setVidVerificationStatementSelector :: Selector
setVidVerificationStatementSelector = mkSelector "setVidVerificationStatement:"

-- | @Selector@ for @vvsc@
vvscSelector :: Selector
vvscSelector = mkSelector "vvsc"

-- | @Selector@ for @setVvsc:@
setVvscSelector :: Selector
setVvscSelector = mkSelector "setVvsc:"

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

