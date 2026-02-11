{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Exposes Matter-specific information from an operational X.509 certificate.
--
-- Note: This class does not support parsing certificates related to Device Attestation.
--
-- Generated bindings for @MTRCertificateInfo@.
module ObjC.Matter.MTRCertificateInfo
  ( MTRCertificateInfo
  , IsMTRCertificateInfo(..)
  , new
  , init_
  , initWithTLVBytes
  , issuer
  , subject
  , notBefore
  , notAfter
  , publicKeyData
  , newSelector
  , initSelector
  , initWithTLVBytesSelector
  , issuerSelector
  , subjectSelector
  , notBeforeSelector
  , notAfterSelector
  , publicKeyDataSelector


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

-- | @+ new@
new :: IO (Id MTRCertificateInfo)
new  =
  do
    cls' <- getRequiredClass "MTRCertificateInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRCertificateInfo)
init_ mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes the receiver with an operational certificate in Matter TLV format.
--
-- This can be a node operational certificate, a Matter intermediate certificate, or a Matter root certificate.
--
-- ObjC selector: @- initWithTLVBytes:@
initWithTLVBytes :: (IsMTRCertificateInfo mtrCertificateInfo, IsNSData bytes) => mtrCertificateInfo -> bytes -> IO (Id MTRCertificateInfo)
initWithTLVBytes mtrCertificateInfo  bytes =
  withObjCPtr bytes $ \raw_bytes ->
      sendMsg mtrCertificateInfo (mkSelector "initWithTLVBytes:") (retPtr retVoid) [argPtr (castPtr raw_bytes :: Ptr ())] >>= ownedObject . castPtr

-- | The Distinguished Name of the issuer of the certificate.
--
-- For a node operational certificate, the issuer will match the subject of the root certificate or intermediate certificate that represents the entity that issued the node operational certificate.
--
-- For an intermediate certificate, the issuer will match the subject of the root certificate.
--
-- Matter root certificates are self-signed, i.e. the issuer and the subject are the same.
--
-- ObjC selector: @- issuer@
issuer :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRDistinguishedNameInfo)
issuer mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "issuer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Distinguished Name of the entity represented by the certificate.
--
-- ObjC selector: @- subject@
subject :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRDistinguishedNameInfo)
subject mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "subject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notBefore@
notBefore :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSDate)
notBefore mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "notBefore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notAfter@
notAfter :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSDate)
notAfter mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "notAfter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Public key data for this certificate
--
-- ObjC selector: @- publicKeyData@
publicKeyData :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSData)
publicKeyData mtrCertificateInfo  =
    sendMsg mtrCertificateInfo (mkSelector "publicKeyData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTLVBytes:@
initWithTLVBytesSelector :: Selector
initWithTLVBytesSelector = mkSelector "initWithTLVBytes:"

-- | @Selector@ for @issuer@
issuerSelector :: Selector
issuerSelector = mkSelector "issuer"

-- | @Selector@ for @subject@
subjectSelector :: Selector
subjectSelector = mkSelector "subject"

-- | @Selector@ for @notBefore@
notBeforeSelector :: Selector
notBeforeSelector = mkSelector "notBefore"

-- | @Selector@ for @notAfter@
notAfterSelector :: Selector
notAfterSelector = mkSelector "notAfter"

-- | @Selector@ for @publicKeyData@
publicKeyDataSelector :: Selector
publicKeyDataSelector = mkSelector "publicKeyData"

