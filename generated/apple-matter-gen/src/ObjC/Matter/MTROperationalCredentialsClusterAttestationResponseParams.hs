{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAttestationResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAttestationResponseParams
  ( MTROperationalCredentialsClusterAttestationResponseParams
  , IsMTROperationalCredentialsClusterAttestationResponseParams(..)
  , initWithResponseValue_error
  , attestationElements
  , setAttestationElements
  , attestationSignature
  , setAttestationSignature
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , signature
  , setSignature
  , initWithResponseValue_errorSelector
  , attestationElementsSelector
  , setAttestationElementsSelector
  , attestationSignatureSelector
  , setAttestationSignatureSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , signatureSelector
  , setSignatureSelector


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

-- | Initialize an MTROperationalCredentialsClusterAttestationResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterAttestationResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterAttestationResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterAttestationResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- attestationElements@
attestationElements :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
attestationElements mtrOperationalCredentialsClusterAttestationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "attestationElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttestationElements:@
setAttestationElements :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setAttestationElements mtrOperationalCredentialsClusterAttestationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "setAttestationElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attestationSignature@
attestationSignature :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
attestationSignature mtrOperationalCredentialsClusterAttestationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "attestationSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttestationSignature:@
setAttestationSignature :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setAttestationSignature mtrOperationalCredentialsClusterAttestationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "setAttestationSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- signature@
signature :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
signature mtrOperationalCredentialsClusterAttestationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSignature:@
setSignature :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setSignature mtrOperationalCredentialsClusterAttestationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAttestationResponseParams (mkSelector "setSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @attestationElements@
attestationElementsSelector :: Selector
attestationElementsSelector = mkSelector "attestationElements"

-- | @Selector@ for @setAttestationElements:@
setAttestationElementsSelector :: Selector
setAttestationElementsSelector = mkSelector "setAttestationElements:"

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

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector
setSignatureSelector = mkSelector "setSignature:"

