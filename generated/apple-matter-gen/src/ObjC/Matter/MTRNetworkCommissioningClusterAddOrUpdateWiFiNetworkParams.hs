{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams
  ( MTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams
  , IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams(..)
  , ssid
  , setSsid
  , credentials
  , setCredentials
  , breadcrumb
  , setBreadcrumb
  , networkIdentity
  , setNetworkIdentity
  , clientIdentifier
  , setClientIdentifier
  , possessionNonce
  , setPossessionNonce
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ssidSelector
  , setSsidSelector
  , credentialsSelector
  , setCredentialsSelector
  , breadcrumbSelector
  , setBreadcrumbSelector
  , networkIdentitySelector
  , setNetworkIdentitySelector
  , clientIdentifierSelector
  , setClientIdentifierSelector
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

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "ssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setSsid mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setSsid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
credentials mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setCredentials mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkIdentity@
networkIdentity :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
networkIdentity mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "networkIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkIdentity:@
setNetworkIdentity :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setNetworkIdentity mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setNetworkIdentity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientIdentifier@
clientIdentifier :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
clientIdentifier mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "clientIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientIdentifier:@
setClientIdentifier :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setClientIdentifier mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setClientIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- possessionNonce@
possessionNonce :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSData)
possessionNonce mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "possessionNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPossessionNonce:@
setPossessionNonce :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setPossessionNonce mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setPossessionNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ssid@
ssidSelector :: Selector
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @setSsid:@
setSsidSelector :: Selector
setSsidSelector = mkSelector "setSsid:"

-- | @Selector@ for @credentials@
credentialsSelector :: Selector
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector
setCredentialsSelector = mkSelector "setCredentials:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

-- | @Selector@ for @networkIdentity@
networkIdentitySelector :: Selector
networkIdentitySelector = mkSelector "networkIdentity"

-- | @Selector@ for @setNetworkIdentity:@
setNetworkIdentitySelector :: Selector
setNetworkIdentitySelector = mkSelector "setNetworkIdentity:"

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @setClientIdentifier:@
setClientIdentifierSelector :: Selector
setClientIdentifierSelector = mkSelector "setClientIdentifier:"

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

