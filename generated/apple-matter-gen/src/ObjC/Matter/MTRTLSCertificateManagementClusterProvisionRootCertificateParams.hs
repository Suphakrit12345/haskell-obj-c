{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionRootCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionRootCertificateParams
  ( MTRTLSCertificateManagementClusterProvisionRootCertificateParams
  , IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams(..)
  , certificate
  , setCertificate
  , caid
  , setCaid
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , certificateSelector
  , setCertificateSelector
  , caidSelector
  , setCaidSelector
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

-- | @- certificate@
certificate :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSData)
certificate mtrtlsCertificateManagementClusterProvisionRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "certificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificate:@
setCertificate :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSData value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setCertificate mtrtlsCertificateManagementClusterProvisionRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "setCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterProvisionRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterProvisionRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterProvisionRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams mtrtlsCertificateManagementClusterProvisionRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterProvisionRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @certificate@
certificateSelector :: Selector
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

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

