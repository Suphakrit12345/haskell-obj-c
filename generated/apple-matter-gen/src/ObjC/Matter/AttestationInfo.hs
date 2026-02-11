{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AttestationInfo@.
module ObjC.Matter.AttestationInfo
  ( AttestationInfo
  , IsAttestationInfo(..)
  , initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo
  , challenge
  , setChallenge
  , nonce
  , setNonce
  , elements
  , setElements
  , elementsSignature
  , setElementsSignature
  , dac
  , setDac
  , pai
  , setPai
  , certificationDeclaration
  , setCertificationDeclaration
  , firmwareInfo
  , setFirmwareInfo
  , initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector
  , challengeSelector
  , setChallengeSelector
  , nonceSelector
  , setNonceSelector
  , elementsSelector
  , setElementsSelector
  , elementsSignatureSelector
  , setElementsSignatureSelector
  , dacSelector
  , setDacSelector
  , paiSelector
  , setPaiSelector
  , certificationDeclarationSelector
  , setCertificationDeclarationSelector
  , firmwareInfoSelector
  , setFirmwareInfoSelector


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

-- | @- initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:@
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo :: (IsAttestationInfo attestationInfo, IsNSData challenge, IsNSData nonce, IsNSData elements, IsNSData elementsSignature, IsNSData dac, IsNSData pai, IsNSData certificationDeclaration, IsNSData firmwareInfo) => attestationInfo -> challenge -> nonce -> elements -> elementsSignature -> dac -> pai -> certificationDeclaration -> firmwareInfo -> IO (Id AttestationInfo)
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo attestationInfo  challenge nonce elements elementsSignature dac pai certificationDeclaration firmwareInfo =
  withObjCPtr challenge $ \raw_challenge ->
    withObjCPtr nonce $ \raw_nonce ->
      withObjCPtr elements $ \raw_elements ->
        withObjCPtr elementsSignature $ \raw_elementsSignature ->
          withObjCPtr dac $ \raw_dac ->
            withObjCPtr pai $ \raw_pai ->
              withObjCPtr certificationDeclaration $ \raw_certificationDeclaration ->
                withObjCPtr firmwareInfo $ \raw_firmwareInfo ->
                    sendMsg attestationInfo (mkSelector "initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr raw_elements :: Ptr ()), argPtr (castPtr raw_elementsSignature :: Ptr ()), argPtr (castPtr raw_dac :: Ptr ()), argPtr (castPtr raw_pai :: Ptr ()), argPtr (castPtr raw_certificationDeclaration :: Ptr ()), argPtr (castPtr raw_firmwareInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- challenge@
challenge :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
challenge attestationInfo  =
    sendMsg attestationInfo (mkSelector "challenge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChallenge:@
setChallenge :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setChallenge attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setChallenge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nonce@
nonce :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
nonce attestationInfo  =
    sendMsg attestationInfo (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNonce:@
setNonce :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setNonce attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elements@
elements :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
elements attestationInfo  =
    sendMsg attestationInfo (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElements:@
setElements :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setElements attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elementsSignature@
elementsSignature :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
elementsSignature attestationInfo  =
    sendMsg attestationInfo (mkSelector "elementsSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElementsSignature:@
setElementsSignature :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setElementsSignature attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setElementsSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dac@
dac :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
dac attestationInfo  =
    sendMsg attestationInfo (mkSelector "dac") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDac:@
setDac :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setDac attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setDac:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pai@
pai :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
pai attestationInfo  =
    sendMsg attestationInfo (mkSelector "pai") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPai:@
setPai :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setPai attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setPai:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- certificationDeclaration@
certificationDeclaration :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
certificationDeclaration attestationInfo  =
    sendMsg attestationInfo (mkSelector "certificationDeclaration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificationDeclaration:@
setCertificationDeclaration :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setCertificationDeclaration attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setCertificationDeclaration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- firmwareInfo@
firmwareInfo :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
firmwareInfo attestationInfo  =
    sendMsg attestationInfo (mkSelector "firmwareInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFirmwareInfo:@
setFirmwareInfo :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setFirmwareInfo attestationInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg attestationInfo (mkSelector "setFirmwareInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:@
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector :: Selector
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector = mkSelector "initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @setChallenge:@
setChallengeSelector :: Selector
setChallengeSelector = mkSelector "setChallenge:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector
setElementsSelector = mkSelector "setElements:"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector
elementsSignatureSelector = mkSelector "elementsSignature"

-- | @Selector@ for @setElementsSignature:@
setElementsSignatureSelector :: Selector
setElementsSignatureSelector = mkSelector "setElementsSignature:"

-- | @Selector@ for @dac@
dacSelector :: Selector
dacSelector = mkSelector "dac"

-- | @Selector@ for @setDac:@
setDacSelector :: Selector
setDacSelector = mkSelector "setDac:"

-- | @Selector@ for @pai@
paiSelector :: Selector
paiSelector = mkSelector "pai"

-- | @Selector@ for @setPai:@
setPaiSelector :: Selector
setPaiSelector = mkSelector "setPai:"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @setCertificationDeclaration:@
setCertificationDeclarationSelector :: Selector
setCertificationDeclarationSelector = mkSelector "setCertificationDeclaration:"

-- | @Selector@ for @firmwareInfo@
firmwareInfoSelector :: Selector
firmwareInfoSelector = mkSelector "firmwareInfo"

-- | @Selector@ for @setFirmwareInfo:@
setFirmwareInfoSelector :: Selector
setFirmwareInfoSelector = mkSelector "setFirmwareInfo:"

