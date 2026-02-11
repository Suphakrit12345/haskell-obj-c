{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAddTrustedRootCertificateParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAddTrustedRootCertificateParams
  ( MTROperationalCredentialsClusterAddTrustedRootCertificateParams
  , IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams(..)
  , rootCACertificate
  , setRootCACertificate
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , rootCertificate
  , setRootCertificate
  , rootCACertificateSelector
  , setRootCACertificateSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , rootCertificateSelector
  , setRootCertificateSelector


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

-- | @- rootCACertificate@
rootCACertificate :: IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> IO (Id NSData)
rootCACertificate mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  =
    sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "rootCACertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRootCACertificate:@
setRootCACertificate :: (IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams, IsNSData value) => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> value -> IO ()
setRootCACertificate mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "setRootCACertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  =
    sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  =
    sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rootCertificate@
rootCertificate :: IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> IO (Id NSData)
rootCertificate mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  =
    sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "rootCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRootCertificate:@
setRootCertificate :: (IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams mtrOperationalCredentialsClusterAddTrustedRootCertificateParams, IsNSData value) => mtrOperationalCredentialsClusterAddTrustedRootCertificateParams -> value -> IO ()
setRootCertificate mtrOperationalCredentialsClusterAddTrustedRootCertificateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddTrustedRootCertificateParams (mkSelector "setRootCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rootCACertificate@
rootCACertificateSelector :: Selector
rootCACertificateSelector = mkSelector "rootCACertificate"

-- | @Selector@ for @setRootCACertificate:@
setRootCACertificateSelector :: Selector
setRootCACertificateSelector = mkSelector "setRootCACertificate:"

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

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @setRootCertificate:@
setRootCertificateSelector :: Selector
setRootCertificateSelector = mkSelector "setRootCertificate:"

