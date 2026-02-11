{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterClientCSRResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterClientCSRResponseParams
  ( MTRTLSCertificateManagementClusterClientCSRResponseParams
  , IsMTRTLSCertificateManagementClusterClientCSRResponseParams(..)
  , initWithResponseValue_error
  , ccdid
  , setCcdid
  , csr
  , setCsr
  , nonceSignature
  , setNonceSignature
  , initWithResponseValue_errorSelector
  , ccdidSelector
  , setCcdidSelector
  , csrSelector
  , setCsrSelector
  , nonceSignatureSelector
  , setNonceSignatureSelector


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

-- | Initialize an MTRTLSCertificateManagementClusterClientCSRResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterClientCSRResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterClientCSRResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterClientCSRResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterClientCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- csr@
csr :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSData)
csr mtrtlsCertificateManagementClusterClientCSRResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "csr") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCsr:@
setCsr :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSData value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setCsr mtrtlsCertificateManagementClusterClientCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "setCsr:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nonceSignature@
nonceSignature :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSData)
nonceSignature mtrtlsCertificateManagementClusterClientCSRResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "nonceSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNonceSignature:@
setNonceSignature :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSData value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setNonceSignature mtrtlsCertificateManagementClusterClientCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterClientCSRResponseParams (mkSelector "setNonceSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @csr@
csrSelector :: Selector
csrSelector = mkSelector "csr"

-- | @Selector@ for @setCsr:@
setCsrSelector :: Selector
setCsrSelector = mkSelector "setCsr:"

-- | @Selector@ for @nonceSignature@
nonceSignatureSelector :: Selector
nonceSignatureSelector = mkSelector "nonceSignature"

-- | @Selector@ for @setNonceSignature:@
setNonceSignatureSelector :: Selector
setNonceSignatureSelector = mkSelector "setNonceSignature:"

