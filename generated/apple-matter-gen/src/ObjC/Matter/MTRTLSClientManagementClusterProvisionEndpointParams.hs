{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterProvisionEndpointParams@.
module ObjC.Matter.MTRTLSClientManagementClusterProvisionEndpointParams
  ( MTRTLSClientManagementClusterProvisionEndpointParams
  , IsMTRTLSClientManagementClusterProvisionEndpointParams(..)
  , hostname
  , setHostname
  , port
  , setPort
  , caid
  , setCaid
  , ccdid
  , setCcdid
  , endpointID
  , setEndpointID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , hostnameSelector
  , setHostnameSelector
  , portSelector
  , setPortSelector
  , caidSelector
  , setCaidSelector
  , ccdidSelector
  , setCcdidSelector
  , endpointIDSelector
  , setEndpointIDSelector
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

-- | @- hostname@
hostname :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSData)
hostname mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "hostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHostname:@
setHostname :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSData value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setHostname mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setHostname:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- port@
port :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
port mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPort:@
setPort :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setPort mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setPort:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caid@
caid :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
caid mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setCaid mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ccdid@
ccdid :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
ccdid mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setCcdid mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsClientManagementClusterProvisionEndpointParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsClientManagementClusterProvisionEndpointParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hostname@
hostnameSelector :: Selector
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @setHostname:@
setHostnameSelector :: Selector
setHostnameSelector = mkSelector "setHostname:"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

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

