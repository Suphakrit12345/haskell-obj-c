{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceAttestationDeviceInfo@.
module ObjC.Matter.MTRDeviceAttestationDeviceInfo
  ( MTRDeviceAttestationDeviceInfo
  , IsMTRDeviceAttestationDeviceInfo(..)
  , init_
  , new
  , vendorID
  , productID
  , basicInformationVendorID
  , basicInformationProductID
  , dacCertificate
  , dacPAICertificate
  , certificateDeclaration
  , attestationChallenge
  , attestationNonce
  , elementsTLV
  , certificationDeclaration
  , elementsSignature
  , initSelector
  , newSelector
  , vendorIDSelector
  , productIDSelector
  , basicInformationVendorIDSelector
  , basicInformationProductIDSelector
  , dacCertificateSelector
  , dacPAICertificateSelector
  , certificateDeclarationSelector
  , attestationChallengeSelector
  , attestationNonceSelector
  , elementsTLVSelector
  , certificationDeclarationSelector
  , elementsSignatureSelector


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

-- | @- init@
init_ :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id MTRDeviceAttestationDeviceInfo)
init_ mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceAttestationDeviceInfo)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceAttestationDeviceInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The vendor ID from the Device Attestation Certificate. May be nil only if attestation verification failed.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
vendorID mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The product ID from the Device Attestation Certificate. May be nil only if attestation verification failed.
--
-- ObjC selector: @- productID@
productID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
productID mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The vendor ID value from the device's Basic Information cluster that was used for device attestation.  If attestation succeeds, this must match the vendor ID from the certification declaration.
--
-- ObjC selector: @- basicInformationVendorID@
basicInformationVendorID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
basicInformationVendorID mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "basicInformationVendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The product ID value from the device's Basic Information cluster that was used for device attestation.  If attestation succeeds, this must match one of the product IDs from the certification declaration.
--
-- ObjC selector: @- basicInformationProductID@
basicInformationProductID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
basicInformationProductID mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "basicInformationProductID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dacCertificate@
dacCertificate :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
dacCertificate mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "dacCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dacPAICertificate@
dacPAICertificate :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
dacPAICertificate mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "dacPAICertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- certificateDeclaration@
certificateDeclaration :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
certificateDeclaration mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "certificateDeclaration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The attestation challenge from the secure session.
--
-- ObjC selector: @- attestationChallenge@
attestationChallenge :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
attestationChallenge mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "attestationChallenge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The attestation nonce from the AttestationRequest command.
--
-- ObjC selector: @- attestationNonce@
attestationNonce :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
attestationNonce mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "attestationNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The TLV-encoded attestation_elements_message that was used to find the certificationDeclaration (possibly unsuccessfully).
--
-- ObjC selector: @- elementsTLV@
elementsTLV :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
elementsTLV mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "elementsTLV") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The certification declaration of the device, if available.  This is a DER-encoded string representing a CMS-formatted certification declaration.  May be nil only if attestation verification failed.
--
-- ObjC selector: @- certificationDeclaration@
certificationDeclaration :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
certificationDeclaration mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "certificationDeclaration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A signature, using the device attestation private key of the device that sent the attestation information, over the concatenation of elementsTLV and attestationChallenge.
--
-- ObjC selector: @- elementsSignature@
elementsSignature :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
elementsSignature mtrDeviceAttestationDeviceInfo  =
    sendMsg mtrDeviceAttestationDeviceInfo (mkSelector "elementsSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @basicInformationVendorID@
basicInformationVendorIDSelector :: Selector
basicInformationVendorIDSelector = mkSelector "basicInformationVendorID"

-- | @Selector@ for @basicInformationProductID@
basicInformationProductIDSelector :: Selector
basicInformationProductIDSelector = mkSelector "basicInformationProductID"

-- | @Selector@ for @dacCertificate@
dacCertificateSelector :: Selector
dacCertificateSelector = mkSelector "dacCertificate"

-- | @Selector@ for @dacPAICertificate@
dacPAICertificateSelector :: Selector
dacPAICertificateSelector = mkSelector "dacPAICertificate"

-- | @Selector@ for @certificateDeclaration@
certificateDeclarationSelector :: Selector
certificateDeclarationSelector = mkSelector "certificateDeclaration"

-- | @Selector@ for @attestationChallenge@
attestationChallengeSelector :: Selector
attestationChallengeSelector = mkSelector "attestationChallenge"

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @elementsTLV@
elementsTLVSelector :: Selector
elementsTLVSelector = mkSelector "elementsTLV"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector
elementsSignatureSelector = mkSelector "elementsSignature"

