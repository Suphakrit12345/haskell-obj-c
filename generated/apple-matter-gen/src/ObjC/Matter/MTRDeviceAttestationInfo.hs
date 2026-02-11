{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents information relating to product attestation.
--
-- Generated bindings for @MTRDeviceAttestationInfo@.
module ObjC.Matter.MTRDeviceAttestationInfo
  ( MTRDeviceAttestationInfo
  , IsMTRDeviceAttestationInfo(..)
  , initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo
  , challenge
  , nonce
  , elementsTLV
  , elementsSignature
  , deviceAttestationCertificate
  , productAttestationIntermediateCertificate
  , certificationDeclaration
  , firmwareInfo
  , initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector
  , challengeSelector
  , nonceSelector
  , elementsTLVSelector
  , elementsSignatureSelector
  , deviceAttestationCertificateSelector
  , productAttestationIntermediateCertificateSelector
  , certificationDeclarationSelector
  , firmwareInfoSelector


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

-- | @- initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:@
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo :: (IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo, IsNSData challenge, IsNSData nonce, IsNSData elementsTLV, IsNSData elementsSignature, IsNSData deviceAttestationCertificate, IsNSData processAttestationIntermediateCertificate, IsNSData certificationDeclaration, IsNSData firmwareInfo) => mtrDeviceAttestationInfo -> challenge -> nonce -> elementsTLV -> elementsSignature -> deviceAttestationCertificate -> processAttestationIntermediateCertificate -> certificationDeclaration -> firmwareInfo -> IO (Id MTRDeviceAttestationInfo)
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo mtrDeviceAttestationInfo  challenge nonce elementsTLV elementsSignature deviceAttestationCertificate processAttestationIntermediateCertificate certificationDeclaration firmwareInfo =
  withObjCPtr challenge $ \raw_challenge ->
    withObjCPtr nonce $ \raw_nonce ->
      withObjCPtr elementsTLV $ \raw_elementsTLV ->
        withObjCPtr elementsSignature $ \raw_elementsSignature ->
          withObjCPtr deviceAttestationCertificate $ \raw_deviceAttestationCertificate ->
            withObjCPtr processAttestationIntermediateCertificate $ \raw_processAttestationIntermediateCertificate ->
              withObjCPtr certificationDeclaration $ \raw_certificationDeclaration ->
                withObjCPtr firmwareInfo $ \raw_firmwareInfo ->
                    sendMsg mtrDeviceAttestationInfo (mkSelector "initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr raw_elementsTLV :: Ptr ()), argPtr (castPtr raw_elementsSignature :: Ptr ()), argPtr (castPtr raw_deviceAttestationCertificate :: Ptr ()), argPtr (castPtr raw_processAttestationIntermediateCertificate :: Ptr ()), argPtr (castPtr raw_certificationDeclaration :: Ptr ()), argPtr (castPtr raw_firmwareInfo :: Ptr ())] >>= ownedObject . castPtr

-- | The attestation challenge from the secure session.
--
-- ObjC selector: @- challenge@
challenge :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
challenge mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "challenge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The attestation nonce from the AttestationRequest command.
--
-- ObjC selector: @- nonce@
nonce :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
nonce mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The TLV-encoded attestation_elements_message that was used to find the certificationDeclaration and firmwareInfo.
--
-- ObjC selector: @- elementsTLV@
elementsTLV :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
elementsTLV mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "elementsTLV") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A signature, using the device attestation private key of the device that sent the attestation information, over the concatenation of elementsTLV and the attestation challenge from the secure session.
--
-- ObjC selector: @- elementsSignature@
elementsSignature :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
elementsSignature mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "elementsSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The device attestation certificate for the device.  This can be used to verify signatures created with the device attestation private key.
--
-- ObjC selector: @- deviceAttestationCertificate@
deviceAttestationCertificate :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
deviceAttestationCertificate mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "deviceAttestationCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The product attestation intermediate certificate that can be used to verify the authenticity of the device attestation certificate.
--
-- ObjC selector: @- productAttestationIntermediateCertificate@
productAttestationIntermediateCertificate :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
productAttestationIntermediateCertificate mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "productAttestationIntermediateCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The certification declaration of the device.  This is a DER-encoded string representing a CMS-formatted certification declaration.
--
-- ObjC selector: @- certificationDeclaration@
certificationDeclaration :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
certificationDeclaration mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "certificationDeclaration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firmwareInfo@
firmwareInfo :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
firmwareInfo mtrDeviceAttestationInfo  =
    sendMsg mtrDeviceAttestationInfo (mkSelector "firmwareInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:@
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector :: Selector
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector = mkSelector "initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @elementsTLV@
elementsTLVSelector :: Selector
elementsTLVSelector = mkSelector "elementsTLV"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector
elementsSignatureSelector = mkSelector "elementsSignature"

-- | @Selector@ for @deviceAttestationCertificate@
deviceAttestationCertificateSelector :: Selector
deviceAttestationCertificateSelector = mkSelector "deviceAttestationCertificate"

-- | @Selector@ for @productAttestationIntermediateCertificate@
productAttestationIntermediateCertificateSelector :: Selector
productAttestationIntermediateCertificateSelector = mkSelector "productAttestationIntermediateCertificate"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @firmwareInfo@
firmwareInfoSelector :: Selector
firmwareInfoSelector = mkSelector "firmwareInfo"

