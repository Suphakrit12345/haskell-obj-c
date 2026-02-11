{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Parameters that can be used to initialize an MTRDeviceController which has a node identity.
--
-- Generated bindings for @MTRDeviceControllerParameters@.
module ObjC.Matter.MTRDeviceControllerParameters
  ( MTRDeviceControllerParameters
  , IsMTRDeviceControllerParameters(..)
  , setOperationalCertificateIssuer_queue
  , setOTAProviderDelegate_queue
  , productAttestationAuthorityCertificates
  , setProductAttestationAuthorityCertificates
  , certificationDeclarationCertificates
  , setCertificationDeclarationCertificates
  , shouldAdvertiseOperational
  , setShouldAdvertiseOperational
  , concurrentSubscriptionEstablishmentsAllowedOnThread
  , setConcurrentSubscriptionEstablishmentsAllowedOnThread
  , storageBehaviorConfiguration
  , setStorageBehaviorConfiguration
  , setOperationalCertificateIssuer_queueSelector
  , setOTAProviderDelegate_queueSelector
  , productAttestationAuthorityCertificatesSelector
  , setProductAttestationAuthorityCertificatesSelector
  , certificationDeclarationCertificatesSelector
  , setCertificationDeclarationCertificatesSelector
  , shouldAdvertiseOperationalSelector
  , setShouldAdvertiseOperationalSelector
  , concurrentSubscriptionEstablishmentsAllowedOnThreadSelector
  , setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector
  , storageBehaviorConfigurationSelector
  , setStorageBehaviorConfigurationSelector


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

-- | Set an MTROperationalCertificateIssuer to call (on the provided queue) when operational certificates need to be provided during commissioning.
--
-- ObjC selector: @- setOperationalCertificateIssuer:queue:@
setOperationalCertificateIssuer_queue :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSObject queue) => mtrDeviceControllerParameters -> RawId -> queue -> IO ()
setOperationalCertificateIssuer_queue mtrDeviceControllerParameters  operationalCertificateIssuer queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceControllerParameters (mkSelector "setOperationalCertificateIssuer:queue:") retVoid [argPtr (castPtr (unRawId operationalCertificateIssuer) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Set an MTROTAProviderDelegate to call (on the provided queue).  Only needs to be called if this controller should be able to handle OTA for devices.
--
-- ObjC selector: @- setOTAProviderDelegate:queue:@
setOTAProviderDelegate_queue :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSObject queue) => mtrDeviceControllerParameters -> RawId -> queue -> IO ()
setOTAProviderDelegate_queue mtrDeviceControllerParameters  otaProviderDelegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceControllerParameters (mkSelector "setOTAProviderDelegate:queue:") retVoid [argPtr (castPtr (unRawId otaProviderDelegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | The Product Attestation Authority certificates that are trusted to sign device attestation information (and in particular to sign Product Attestation Intermediate certificates, which then sign Device Attestation Certificates).
--
-- Defaults to nil.
--
-- ObjC selector: @- productAttestationAuthorityCertificates@
productAttestationAuthorityCertificates :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id NSArray)
productAttestationAuthorityCertificates mtrDeviceControllerParameters  =
    sendMsg mtrDeviceControllerParameters (mkSelector "productAttestationAuthorityCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Product Attestation Authority certificates that are trusted to sign device attestation information (and in particular to sign Product Attestation Intermediate certificates, which then sign Device Attestation Certificates).
--
-- Defaults to nil.
--
-- ObjC selector: @- setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificates :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSArray value) => mtrDeviceControllerParameters -> value -> IO ()
setProductAttestationAuthorityCertificates mtrDeviceControllerParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerParameters (mkSelector "setProductAttestationAuthorityCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Certification Declaration certificates whose public keys correspond to private keys that are trusted to sign certification declarations.  Defaults to nil.
--
-- These certificates are used in addition to, not replacing, the default set of well-known certification declaration signing keys.
--
-- ObjC selector: @- certificationDeclarationCertificates@
certificationDeclarationCertificates :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id NSArray)
certificationDeclarationCertificates mtrDeviceControllerParameters  =
    sendMsg mtrDeviceControllerParameters (mkSelector "certificationDeclarationCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Certification Declaration certificates whose public keys correspond to private keys that are trusted to sign certification declarations.  Defaults to nil.
--
-- These certificates are used in addition to, not replacing, the default set of well-known certification declaration signing keys.
--
-- ObjC selector: @- setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificates :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSArray value) => mtrDeviceControllerParameters -> value -> IO ()
setCertificationDeclarationCertificates mtrDeviceControllerParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerParameters (mkSelector "setCertificationDeclarationCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether the controller should advertise its operational identity.  Defaults to NO.
--
-- ObjC selector: @- shouldAdvertiseOperational@
shouldAdvertiseOperational :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO Bool
shouldAdvertiseOperational mtrDeviceControllerParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceControllerParameters (mkSelector "shouldAdvertiseOperational") retCULong []

-- | Whether the controller should advertise its operational identity.  Defaults to NO.
--
-- ObjC selector: @- setShouldAdvertiseOperational:@
setShouldAdvertiseOperational :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> Bool -> IO ()
setShouldAdvertiseOperational mtrDeviceControllerParameters  value =
    sendMsg mtrDeviceControllerParameters (mkSelector "setShouldAdvertiseOperational:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the maximum simultaneous subscription establishments that can be happening at one time for devices on Thread. This defaults to a large number.
--
-- If this value is 0, the maximum subscription establishments allowed at a time will be set to 1.
--
-- ObjC selector: @- concurrentSubscriptionEstablishmentsAllowedOnThread@
concurrentSubscriptionEstablishmentsAllowedOnThread :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO CULong
concurrentSubscriptionEstablishmentsAllowedOnThread mtrDeviceControllerParameters  =
    sendMsg mtrDeviceControllerParameters (mkSelector "concurrentSubscriptionEstablishmentsAllowedOnThread") retCULong []

-- | Sets the maximum simultaneous subscription establishments that can be happening at one time for devices on Thread. This defaults to a large number.
--
-- If this value is 0, the maximum subscription establishments allowed at a time will be set to 1.
--
-- ObjC selector: @- setConcurrentSubscriptionEstablishmentsAllowedOnThread:@
setConcurrentSubscriptionEstablishmentsAllowedOnThread :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> CULong -> IO ()
setConcurrentSubscriptionEstablishmentsAllowedOnThread mtrDeviceControllerParameters  value =
    sendMsg mtrDeviceControllerParameters (mkSelector "setConcurrentSubscriptionEstablishmentsAllowedOnThread:") retVoid [argCULong value]

-- | Sets the storage behavior configuration - see MTRDeviceStorageBehaviorConfiguration.h for details
--
-- If this value is nil, a default storage behavior configuration will be used.
--
-- ObjC selector: @- storageBehaviorConfiguration@
storageBehaviorConfiguration :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id MTRDeviceStorageBehaviorConfiguration)
storageBehaviorConfiguration mtrDeviceControllerParameters  =
    sendMsg mtrDeviceControllerParameters (mkSelector "storageBehaviorConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the storage behavior configuration - see MTRDeviceStorageBehaviorConfiguration.h for details
--
-- If this value is nil, a default storage behavior configuration will be used.
--
-- ObjC selector: @- setStorageBehaviorConfiguration:@
setStorageBehaviorConfiguration :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsMTRDeviceStorageBehaviorConfiguration value) => mtrDeviceControllerParameters -> value -> IO ()
setStorageBehaviorConfiguration mtrDeviceControllerParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerParameters (mkSelector "setStorageBehaviorConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setOperationalCertificateIssuer:queue:@
setOperationalCertificateIssuer_queueSelector :: Selector
setOperationalCertificateIssuer_queueSelector = mkSelector "setOperationalCertificateIssuer:queue:"

-- | @Selector@ for @setOTAProviderDelegate:queue:@
setOTAProviderDelegate_queueSelector :: Selector
setOTAProviderDelegate_queueSelector = mkSelector "setOTAProviderDelegate:queue:"

-- | @Selector@ for @productAttestationAuthorityCertificates@
productAttestationAuthorityCertificatesSelector :: Selector
productAttestationAuthorityCertificatesSelector = mkSelector "productAttestationAuthorityCertificates"

-- | @Selector@ for @setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificatesSelector :: Selector
setProductAttestationAuthorityCertificatesSelector = mkSelector "setProductAttestationAuthorityCertificates:"

-- | @Selector@ for @certificationDeclarationCertificates@
certificationDeclarationCertificatesSelector :: Selector
certificationDeclarationCertificatesSelector = mkSelector "certificationDeclarationCertificates"

-- | @Selector@ for @setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificatesSelector :: Selector
setCertificationDeclarationCertificatesSelector = mkSelector "setCertificationDeclarationCertificates:"

-- | @Selector@ for @shouldAdvertiseOperational@
shouldAdvertiseOperationalSelector :: Selector
shouldAdvertiseOperationalSelector = mkSelector "shouldAdvertiseOperational"

-- | @Selector@ for @setShouldAdvertiseOperational:@
setShouldAdvertiseOperationalSelector :: Selector
setShouldAdvertiseOperationalSelector = mkSelector "setShouldAdvertiseOperational:"

-- | @Selector@ for @concurrentSubscriptionEstablishmentsAllowedOnThread@
concurrentSubscriptionEstablishmentsAllowedOnThreadSelector :: Selector
concurrentSubscriptionEstablishmentsAllowedOnThreadSelector = mkSelector "concurrentSubscriptionEstablishmentsAllowedOnThread"

-- | @Selector@ for @setConcurrentSubscriptionEstablishmentsAllowedOnThread:@
setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector :: Selector
setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector = mkSelector "setConcurrentSubscriptionEstablishmentsAllowedOnThread:"

-- | @Selector@ for @storageBehaviorConfiguration@
storageBehaviorConfigurationSelector :: Selector
storageBehaviorConfigurationSelector = mkSelector "storageBehaviorConfiguration"

-- | @Selector@ for @setStorageBehaviorConfiguration:@
setStorageBehaviorConfigurationSelector :: Selector
setStorageBehaviorConfigurationSelector = mkSelector "setStorageBehaviorConfiguration:"

