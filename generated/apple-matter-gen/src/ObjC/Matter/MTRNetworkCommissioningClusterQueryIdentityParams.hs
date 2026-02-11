{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterQueryIdentityParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterQueryIdentityParams
  ( MTRNetworkCommissioningClusterQueryIdentityParams
  , IsMTRNetworkCommissioningClusterQueryIdentityParams(..)
  , keyIdentifier
  , setKeyIdentifier
  , possessionNonce
  , setPossessionNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , keyIdentifierSelector
  , setKeyIdentifierSelector
  , possessionNonceSelector
  , setPossessionNonceSelector
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

-- | @- keyIdentifier@
keyIdentifier :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSData)
keyIdentifier mtrNetworkCommissioningClusterQueryIdentityParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "keyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyIdentifier:@
setKeyIdentifier :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setKeyIdentifier mtrNetworkCommissioningClusterQueryIdentityParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "setKeyIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- possessionNonce@
possessionNonce :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSData)
possessionNonce mtrNetworkCommissioningClusterQueryIdentityParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "possessionNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPossessionNonce:@
setPossessionNonce :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setPossessionNonce mtrNetworkCommissioningClusterQueryIdentityParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "setPossessionNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterQueryIdentityParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSNumber value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterQueryIdentityParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams => mtrNetworkCommissioningClusterQueryIdentityParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterQueryIdentityParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterQueryIdentityParams mtrNetworkCommissioningClusterQueryIdentityParams, IsNSNumber value) => mtrNetworkCommissioningClusterQueryIdentityParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterQueryIdentityParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @setKeyIdentifier:@
setKeyIdentifierSelector :: Selector
setKeyIdentifierSelector = mkSelector "setKeyIdentifier:"

-- | @Selector@ for @possessionNonce@
possessionNonceSelector :: Selector
possessionNonceSelector = mkSelector "possessionNonce"

-- | @Selector@ for @setPossessionNonce:@
setPossessionNonceSelector :: Selector
setPossessionNonceSelector = mkSelector "setPossessionNonce:"

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

