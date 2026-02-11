{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionClientCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionClientCertificateParams
  ( MTRTLSCertificateManagementClusterProvisionClientCertificateParams
  , IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams(..)
  , ccdid
  , setCcdid
  , clientCertificate
  , setClientCertificate
  , intermediateCertificates
  , setIntermediateCertificates
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ccdidSelector
  , setCcdidSelector
  , clientCertificateSelector
  , setClientCertificateSelector
  , intermediateCertificatesSelector
  , setIntermediateCertificatesSelector
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

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterProvisionClientCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterProvisionClientCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientCertificate@
clientCertificate :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSData)
clientCertificate mtrtlsCertificateManagementClusterProvisionClientCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "clientCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientCertificate:@
setClientCertificate :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setClientCertificate mtrtlsCertificateManagementClusterProvisionClientCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "setClientCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intermediateCertificates@
intermediateCertificates :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSArray)
intermediateCertificates mtrtlsCertificateManagementClusterProvisionClientCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "intermediateCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIntermediateCertificates:@
setIntermediateCertificates :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSArray value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setIntermediateCertificates mtrtlsCertificateManagementClusterProvisionClientCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "setIntermediateCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionClientCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionClientCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionClientCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams mtrtlsCertificateManagementClusterProvisionClientCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionClientCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionClientCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionClientCertificateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @clientCertificate@
clientCertificateSelector :: Selector
clientCertificateSelector = mkSelector "clientCertificate"

-- | @Selector@ for @setClientCertificate:@
setClientCertificateSelector :: Selector
setClientCertificateSelector = mkSelector "setClientCertificate:"

-- | @Selector@ for @intermediateCertificates@
intermediateCertificatesSelector :: Selector
intermediateCertificatesSelector = mkSelector "intermediateCertificates"

-- | @Selector@ for @setIntermediateCertificates:@
setIntermediateCertificatesSelector :: Selector
setIntermediateCertificatesSelector = mkSelector "setIntermediateCertificates:"

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

