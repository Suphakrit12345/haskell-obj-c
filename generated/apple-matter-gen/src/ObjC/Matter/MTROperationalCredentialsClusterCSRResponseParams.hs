{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterCSRResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterCSRResponseParams
  ( MTROperationalCredentialsClusterCSRResponseParams
  , IsMTROperationalCredentialsClusterCSRResponseParams(..)
  , initWithResponseValue_error
  , nocsrElements
  , setNocsrElements
  , attestationSignature
  , setAttestationSignature
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , nocsrElementsSelector
  , setNocsrElementsSelector
  , attestationSignatureSelector
  , setAttestationSignatureSelector
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

-- | Initialize an MTROperationalCredentialsClusterCSRResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterCSRResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterCSRResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterCSRResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- nocsrElements@
nocsrElements :: IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams => mtrOperationalCredentialsClusterCSRResponseParams -> IO (Id NSData)
nocsrElements mtrOperationalCredentialsClusterCSRResponseParams  =
    sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "nocsrElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNocsrElements:@
setNocsrElements :: (IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams, IsNSData value) => mtrOperationalCredentialsClusterCSRResponseParams -> value -> IO ()
setNocsrElements mtrOperationalCredentialsClusterCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "setNocsrElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attestationSignature@
attestationSignature :: IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams => mtrOperationalCredentialsClusterCSRResponseParams -> IO (Id NSData)
attestationSignature mtrOperationalCredentialsClusterCSRResponseParams  =
    sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "attestationSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttestationSignature:@
setAttestationSignature :: (IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams, IsNSData value) => mtrOperationalCredentialsClusterCSRResponseParams -> value -> IO ()
setAttestationSignature mtrOperationalCredentialsClusterCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "setAttestationSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams => mtrOperationalCredentialsClusterCSRResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRResponseParams  =
    sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterCSRResponseParams mtrOperationalCredentialsClusterCSRResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterCSRResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @nocsrElements@
nocsrElementsSelector :: Selector
nocsrElementsSelector = mkSelector "nocsrElements"

-- | @Selector@ for @setNocsrElements:@
setNocsrElementsSelector :: Selector
setNocsrElementsSelector = mkSelector "setNocsrElements:"

-- | @Selector@ for @attestationSignature@
attestationSignatureSelector :: Selector
attestationSignatureSelector = mkSelector "attestationSignature"

-- | @Selector@ for @setAttestationSignature:@
setAttestationSignatureSelector :: Selector
setAttestationSignatureSelector = mkSelector "setAttestationSignature:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

