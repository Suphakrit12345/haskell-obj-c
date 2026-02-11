{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAttestationRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAttestationRequestParams
  ( MTROperationalCredentialsClusterAttestationRequestParams
  , IsMTROperationalCredentialsClusterAttestationRequestParams(..)
  , attestationNonce
  , setAttestationNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , attestationNonceSelector
  , setAttestationNonceSelector
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

-- | @- attestationNonce@
attestationNonce :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSData)
attestationNonce mtrOperationalCredentialsClusterAttestationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "attestationNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttestationNonce:@
setAttestationNonce :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setAttestationNonce mtrOperationalCredentialsClusterAttestationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "setAttestationNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams => mtrOperationalCredentialsClusterAttestationRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterAttestationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterAttestationRequestParams mtrOperationalCredentialsClusterAttestationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterAttestationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @setAttestationNonce:@
setAttestationNonceSelector :: Selector
setAttestationNonceSelector = mkSelector "setAttestationNonce:"

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

