{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterCSRRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterCSRRequestParams
  ( MTROperationalCredentialsClusterCSRRequestParams
  , IsMTROperationalCredentialsClusterCSRRequestParams(..)
  , csrNonce
  , setCsrNonce
  , isForUpdateNOC
  , setIsForUpdateNOC
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , csrNonceSelector
  , setCsrNonceSelector
  , isForUpdateNOCSelector
  , setIsForUpdateNOCSelector
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

-- | @- csrNonce@
csrNonce :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSData)
csrNonce mtrOperationalCredentialsClusterCSRRequestParams  =
    sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "csrNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCsrNonce:@
setCsrNonce :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSData value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setCsrNonce mtrOperationalCredentialsClusterCSRRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "setCsrNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isForUpdateNOC@
isForUpdateNOC :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
isForUpdateNOC mtrOperationalCredentialsClusterCSRRequestParams  =
    sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "isForUpdateNOC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIsForUpdateNOC:@
setIsForUpdateNOC :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setIsForUpdateNOC mtrOperationalCredentialsClusterCSRRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "setIsForUpdateNOC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRRequestParams  =
    sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterCSRRequestParams  =
    sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterCSRRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @csrNonce@
csrNonceSelector :: Selector
csrNonceSelector = mkSelector "csrNonce"

-- | @Selector@ for @setCsrNonce:@
setCsrNonceSelector :: Selector
setCsrNonceSelector = mkSelector "setCsrNonce:"

-- | @Selector@ for @isForUpdateNOC@
isForUpdateNOCSelector :: Selector
isForUpdateNOCSelector = mkSelector "isForUpdateNOC"

-- | @Selector@ for @setIsForUpdateNOC:@
setIsForUpdateNOCSelector :: Selector
setIsForUpdateNOCSelector = mkSelector "setIsForUpdateNOC:"

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

