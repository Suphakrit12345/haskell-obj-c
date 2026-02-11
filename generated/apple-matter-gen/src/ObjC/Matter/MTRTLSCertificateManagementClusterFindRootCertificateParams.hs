{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindRootCertificateParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindRootCertificateParams
  ( MTRTLSCertificateManagementClusterFindRootCertificateParams
  , IsMTRTLSCertificateManagementClusterFindRootCertificateParams(..)
  , caid
  , setCaid
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams => mtrtlsCertificateManagementClusterFindRootCertificateParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterFindRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindRootCertificateParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterFindRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams => mtrtlsCertificateManagementClusterFindRootCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsCertificateManagementClusterFindRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindRootCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsCertificateManagementClusterFindRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams => mtrtlsCertificateManagementClusterFindRootCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsCertificateManagementClusterFindRootCertificateParams  =
    sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSCertificateManagementClusterFindRootCertificateParams mtrtlsCertificateManagementClusterFindRootCertificateParams, IsNSNumber value) => mtrtlsCertificateManagementClusterFindRootCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsCertificateManagementClusterFindRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterFindRootCertificateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

