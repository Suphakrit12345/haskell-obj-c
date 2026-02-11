{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterCertificateChainResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterCertificateChainResponseParams
  ( MTROperationalCredentialsClusterCertificateChainResponseParams
  , IsMTROperationalCredentialsClusterCertificateChainResponseParams(..)
  , initWithResponseValue_error
  , certificate
  , setCertificate
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , certificateSelector
  , setCertificateSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector


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

-- | Initialize an MTROperationalCredentialsClusterCertificateChainResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterCertificateChainResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterCertificateChainResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalCredentialsClusterCertificateChainResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- certificate@
certificate :: IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams => mtrOperationalCredentialsClusterCertificateChainResponseParams -> IO (Id NSData)
certificate mtrOperationalCredentialsClusterCertificateChainResponseParams  =
    sendMsg mtrOperationalCredentialsClusterCertificateChainResponseParams (mkSelector "certificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificate:@
setCertificate :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSData value) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> value -> IO ()
setCertificate mtrOperationalCredentialsClusterCertificateChainResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCertificateChainResponseParams (mkSelector "setCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams => mtrOperationalCredentialsClusterCertificateChainResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterCertificateChainResponseParams  =
    sendMsg mtrOperationalCredentialsClusterCertificateChainResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterCertificateChainResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCertificateChainResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

