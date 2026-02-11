{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Matter Onboarding Payload.
--
-- It can be represented as a numeric Manual Pairing Code or as QR Code. The QR Code format contains more information though, so creating a QR Code from a payload that was initialized from a Manual Pairing Code will not work, because some required information will be missing.
--
-- This class can also be used to create an onboarding payload directly from the underlying values (passcode, discriminator, etc).
--
-- Generated bindings for @MTRSetupPayload@.
module ObjC.Matter.MTRSetupPayload
  ( MTRSetupPayload
  , IsMTRSetupPayload(..)
  , initWithPayload
  , vendorElementWithTag
  , removeVendorElementWithTag
  , addOrReplaceVendorElement
  , generateRandomPIN
  , generateRandomSetupPasscode
  , initWithSetupPasscode_discriminator
  , manualEntryCode
  , qrCodeString
  , isValidSetupPasscode
  , init_
  , new
  , setupPayloadWithOnboardingPayload_error
  , getAllOptionalVendorData
  , concatenated
  , subPayloads
  , setSubPayloads
  , version
  , setVersion
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , commissioningFlow
  , setCommissioningFlow
  , discoveryCapabilities
  , setDiscoveryCapabilities
  , discriminator
  , setDiscriminator
  , hasShortDiscriminator
  , setHasShortDiscriminator
  , setupPasscode
  , setSetupPasscode
  , serialNumber
  , setSerialNumber
  , vendorElements
  , rendezvousInformation
  , setRendezvousInformation
  , setUpPINCode
  , setSetUpPINCode
  , initWithPayloadSelector
  , vendorElementWithTagSelector
  , removeVendorElementWithTagSelector
  , addOrReplaceVendorElementSelector
  , generateRandomPINSelector
  , generateRandomSetupPasscodeSelector
  , initWithSetupPasscode_discriminatorSelector
  , manualEntryCodeSelector
  , qrCodeStringSelector
  , isValidSetupPasscodeSelector
  , initSelector
  , newSelector
  , setupPayloadWithOnboardingPayload_errorSelector
  , getAllOptionalVendorDataSelector
  , concatenatedSelector
  , subPayloadsSelector
  , setSubPayloadsSelector
  , versionSelector
  , setVersionSelector
  , vendorIDSelector
  , setVendorIDSelector
  , productIDSelector
  , setProductIDSelector
  , commissioningFlowSelector
  , setCommissioningFlowSelector
  , discoveryCapabilitiesSelector
  , setDiscoveryCapabilitiesSelector
  , discriminatorSelector
  , setDiscriminatorSelector
  , hasShortDiscriminatorSelector
  , setHasShortDiscriminatorSelector
  , setupPasscodeSelector
  , setSetupPasscodeSelector
  , serialNumberSelector
  , setSerialNumberSelector
  , vendorElementsSelector
  , rendezvousInformationSelector
  , setRendezvousInformationSelector
  , setUpPINCodeSelector
  , setSetUpPINCodeSelector

  -- * Enum types
  , MTRCommissioningFlow(MTRCommissioningFlow)
  , pattern MTRCommissioningFlowStandard
  , pattern MTRCommissioningFlowUserActionRequired
  , pattern MTRCommissioningFlowCustom
  , pattern MTRCommissioningFlowInvalid
  , MTRDiscoveryCapabilities(MTRDiscoveryCapabilities)
  , pattern MTRDiscoveryCapabilitiesUnknown
  , pattern MTRDiscoveryCapabilitiesNone
  , pattern MTRDiscoveryCapabilitiesSoftAP
  , pattern MTRDiscoveryCapabilitiesBLE
  , pattern MTRDiscoveryCapabilitiesOnNetwork
  , pattern MTRDiscoveryCapabilitiesNFC
  , pattern MTRDiscoveryCapabilitiesAllMask

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes the payload object from the provide QR Code or Manual Pairing Code string. Returns nil if the payload is not valid.
--
-- ObjC selector: @- initWithPayload:@
initWithPayload :: (IsMTRSetupPayload mtrSetupPayload, IsNSString payload) => mtrSetupPayload -> payload -> IO (Id MTRSetupPayload)
initWithPayload mtrSetupPayload  payload =
  withObjCPtr payload $ \raw_payload ->
      sendMsg mtrSetupPayload (mkSelector "initWithPayload:") (retPtr retVoid) [argPtr (castPtr raw_payload :: Ptr ())] >>= ownedObject . castPtr

-- | Returns the Manufacturer-specific extension element with the specified tag, if any. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- vendorElementWithTag:@
vendorElementWithTag :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber tag) => mtrSetupPayload -> tag -> IO (Id MTROptionalQRCodeInfo)
vendorElementWithTag mtrSetupPayload  tag =
  withObjCPtr tag $ \raw_tag ->
      sendMsg mtrSetupPayload (mkSelector "vendorElementWithTag:") (retPtr retVoid) [argPtr (castPtr raw_tag :: Ptr ())] >>= retainedObject . castPtr

-- | Removes the extension element with the specified tag, if any. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- removeVendorElementWithTag:@
removeVendorElementWithTag :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber tag) => mtrSetupPayload -> tag -> IO ()
removeVendorElementWithTag mtrSetupPayload  tag =
  withObjCPtr tag $ \raw_tag ->
      sendMsg mtrSetupPayload (mkSelector "removeVendorElementWithTag:") retVoid [argPtr (castPtr raw_tag :: Ptr ())]

-- | Adds or replaces a Manufacturer-specific extension element.
--
-- ObjC selector: @- addOrReplaceVendorElement:@
addOrReplaceVendorElement :: (IsMTRSetupPayload mtrSetupPayload, IsMTROptionalQRCodeInfo element) => mtrSetupPayload -> element -> IO ()
addOrReplaceVendorElement mtrSetupPayload  element =
  withObjCPtr element $ \raw_element ->
      sendMsg mtrSetupPayload (mkSelector "addOrReplaceVendorElement:") retVoid [argPtr (castPtr raw_element :: Ptr ())]

-- | Generate a random Matter-valid setup PIN.
--
-- ObjC selector: @+ generateRandomPIN@
generateRandomPIN :: IO CULong
generateRandomPIN  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMsg cls' (mkSelector "generateRandomPIN") retCULong []

-- | Generate a random Matter-valid setup passcode.
--
-- ObjC selector: @+ generateRandomSetupPasscode@
generateRandomSetupPasscode :: IO (Id NSNumber)
generateRandomSetupPasscode  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMsg cls' (mkSelector "generateRandomSetupPasscode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Initialize an MTRSetupPayload with the given passcode and discriminator. This will pre-set version, product id, and vendor id to 0.
--
-- ObjC selector: @- initWithSetupPasscode:discriminator:@
initWithSetupPasscode_discriminator :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber setupPasscode, IsNSNumber discriminator) => mtrSetupPayload -> setupPasscode -> discriminator -> IO (Id MTRSetupPayload)
initWithSetupPasscode_discriminator mtrSetupPayload  setupPasscode discriminator =
  withObjCPtr setupPasscode $ \raw_setupPasscode ->
    withObjCPtr discriminator $ \raw_discriminator ->
        sendMsg mtrSetupPayload (mkSelector "initWithSetupPasscode:discriminator:") (retPtr retVoid) [argPtr (castPtr raw_setupPasscode :: Ptr ()), argPtr (castPtr raw_discriminator :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a Manual Pairing Code from this setup payload. Returns nil if this payload cannot be represented as a valid Manual Pairing Code.
--
-- The following properties must be populated for a valid Manual Pairing Code:  - setupPasscode  - discriminator (short or long)
--
-- In most cases the pairing code will be 11 digits long. If the payload indicates a @commissioningFlow@ other than @MTRCommissioningFlowStandard@, a 21 digit code will be produced that includes the vendorID and productID values.
--
-- ObjC selector: @- manualEntryCode@
manualEntryCode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
manualEntryCode mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "manualEntryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a QR Code payload from this setup payload. Returns nil if this payload cannot be represented as a valid QR Code.
--
-- The following properties must be populated for a valid QR Code: - setupPasscode - discriminator (must be long) - discoveryCapabilities (not MTRDiscoveryCapabilitiesUnknown)
--
-- If this object represents a @concatenated@ payload, then this property will include the QR Code strings of all the underlying @subPayloads.@
--
-- ObjC selector: @- qrCodeString@
qrCodeString :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
qrCodeString mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "qrCodeString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Check whether the provided setup passcode (represented as an unsigned integer) is a valid setup passcode.
--
-- ObjC selector: @+ isValidSetupPasscode:@
isValidSetupPasscode :: IsNSNumber setupPasscode => setupPasscode -> IO Bool
isValidSetupPasscode setupPasscode =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    withObjCPtr setupPasscode $ \raw_setupPasscode ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isValidSetupPasscode:") retCULong [argPtr (castPtr raw_setupPasscode :: Ptr ())]

-- | @- init@
init_ :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id MTRSetupPayload)
init_ mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRSetupPayload)
new  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ setupPayloadWithOnboardingPayload:error:@
setupPayloadWithOnboardingPayload_error :: (IsNSString onboardingPayload, IsNSError error_) => onboardingPayload -> error_ -> IO (Id MTRSetupPayload)
setupPayloadWithOnboardingPayload_error onboardingPayload error_ =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    withObjCPtr onboardingPayload $ \raw_onboardingPayload ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "setupPayloadWithOnboardingPayload:error:") (retPtr retVoid) [argPtr (castPtr raw_onboardingPayload :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- getAllOptionalVendorData:@
getAllOptionalVendorData :: (IsMTRSetupPayload mtrSetupPayload, IsNSError error_) => mtrSetupPayload -> error_ -> IO (Id NSArray)
getAllOptionalVendorData mtrSetupPayload  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mtrSetupPayload (mkSelector "getAllOptionalVendorData:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Whether this object represents a concatenated QR Code payload consisting of two or more underlying payloads. If YES, then:
--
-- - The constituent payloads are exposed in the @subPayloads@ property.
--
-- - Properties other than @subPayloads@ and @qrCodeString@ (e.g. @vendorID@, @discriminator@)   are not relevant to a concatenated payload and should not be used. If accessed, they will   act as if the payload was not in fact concatenated, and return the relevant value associated   with the first sub-payload. Mutating such a property will discard the additional sub-payloads.
--
-- ObjC selector: @- concatenated@
concatenated :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO Bool
concatenated mtrSetupPayload  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrSetupPayload (mkSelector "concatenated") retCULong []

-- | The individual constituent payloads, if the receiver represents a concatenated payload.
--
-- See: concatenated
--
-- ObjC selector: @- subPayloads@
subPayloads :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSArray)
subPayloads mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "subPayloads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The individual constituent payloads, if the receiver represents a concatenated payload.
--
-- See: concatenated
--
-- ObjC selector: @- setSubPayloads:@
setSubPayloads :: (IsMTRSetupPayload mtrSetupPayload, IsNSArray value) => mtrSetupPayload -> value -> IO ()
setSubPayloads mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setSubPayloads:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- version@
version :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
version mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersion:@
setVersion :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setVersion mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorID@
vendorID :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
vendorID mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setVendorID mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productID@
productID :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
productID mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductID:@
setProductID :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setProductID mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setProductID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- commissioningFlow@
commissioningFlow :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO MTRCommissioningFlow
commissioningFlow mtrSetupPayload  =
    fmap (coerce :: CULong -> MTRCommissioningFlow) $ sendMsg mtrSetupPayload (mkSelector "commissioningFlow") retCULong []

-- | @- setCommissioningFlow:@
setCommissioningFlow :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> MTRCommissioningFlow -> IO ()
setCommissioningFlow mtrSetupPayload  value =
    sendMsg mtrSetupPayload (mkSelector "setCommissioningFlow:") retVoid [argCULong (coerce value)]

-- | The value of discoveryCapabilities is made up of the various MTRDiscoveryCapabilities flags.  If the discovery capabilities are not known, this will be set to MTRDiscoveryCapabilitiesUnknown.
--
-- ObjC selector: @- discoveryCapabilities@
discoveryCapabilities :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO MTRDiscoveryCapabilities
discoveryCapabilities mtrSetupPayload  =
    fmap (coerce :: CULong -> MTRDiscoveryCapabilities) $ sendMsg mtrSetupPayload (mkSelector "discoveryCapabilities") retCULong []

-- | The value of discoveryCapabilities is made up of the various MTRDiscoveryCapabilities flags.  If the discovery capabilities are not known, this will be set to MTRDiscoveryCapabilitiesUnknown.
--
-- ObjC selector: @- setDiscoveryCapabilities:@
setDiscoveryCapabilities :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> MTRDiscoveryCapabilities -> IO ()
setDiscoveryCapabilities mtrSetupPayload  value =
    sendMsg mtrSetupPayload (mkSelector "setDiscoveryCapabilities:") retVoid [argCULong (coerce value)]

-- | @- discriminator@
discriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
discriminator mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "discriminator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setDiscriminator mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setDiscriminator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If hasShortDiscriminator is true, the discriminator value contains just the high 4 bits of the full discriminator.  For example, if hasShortDiscriminator is true and discriminator is 0xA, then the full discriminator can be anything in the range 0xA00 to 0xAFF.
--
-- ObjC selector: @- hasShortDiscriminator@
hasShortDiscriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO Bool
hasShortDiscriminator mtrSetupPayload  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrSetupPayload (mkSelector "hasShortDiscriminator") retCULong []

-- | If hasShortDiscriminator is true, the discriminator value contains just the high 4 bits of the full discriminator.  For example, if hasShortDiscriminator is true and discriminator is 0xA, then the full discriminator can be anything in the range 0xA00 to 0xAFF.
--
-- ObjC selector: @- setHasShortDiscriminator:@
setHasShortDiscriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> Bool -> IO ()
setHasShortDiscriminator mtrSetupPayload  value =
    sendMsg mtrSetupPayload (mkSelector "setHasShortDiscriminator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- setupPasscode@
setupPasscode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
setupPasscode mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "setupPasscode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSetupPasscode:@
setSetupPasscode :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setSetupPasscode mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setSetupPasscode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The value of the Serial Number extension element, if any.
--
-- ObjC selector: @- serialNumber@
serialNumber :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
serialNumber mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "serialNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the Serial Number extension element, if any.
--
-- ObjC selector: @- setSerialNumber:@
setSerialNumber :: (IsMTRSetupPayload mtrSetupPayload, IsNSString value) => mtrSetupPayload -> value -> IO ()
setSerialNumber mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setSerialNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The list of Manufacturer-specific extension elements contained in the setup code. May be empty.
--
-- ObjC selector: @- vendorElements@
vendorElements :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSArray)
vendorElements mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "vendorElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rendezvousInformation@
rendezvousInformation :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
rendezvousInformation mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "rendezvousInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRendezvousInformation:@
setRendezvousInformation :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setRendezvousInformation mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setRendezvousInformation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setUpPINCode@
setUpPINCode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
setUpPINCode mtrSetupPayload  =
    sendMsg mtrSetupPayload (mkSelector "setUpPINCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSetUpPINCode:@
setSetUpPINCode :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setSetUpPINCode mtrSetupPayload  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSetupPayload (mkSelector "setSetUpPINCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:@
initWithPayloadSelector :: Selector
initWithPayloadSelector = mkSelector "initWithPayload:"

-- | @Selector@ for @vendorElementWithTag:@
vendorElementWithTagSelector :: Selector
vendorElementWithTagSelector = mkSelector "vendorElementWithTag:"

-- | @Selector@ for @removeVendorElementWithTag:@
removeVendorElementWithTagSelector :: Selector
removeVendorElementWithTagSelector = mkSelector "removeVendorElementWithTag:"

-- | @Selector@ for @addOrReplaceVendorElement:@
addOrReplaceVendorElementSelector :: Selector
addOrReplaceVendorElementSelector = mkSelector "addOrReplaceVendorElement:"

-- | @Selector@ for @generateRandomPIN@
generateRandomPINSelector :: Selector
generateRandomPINSelector = mkSelector "generateRandomPIN"

-- | @Selector@ for @generateRandomSetupPasscode@
generateRandomSetupPasscodeSelector :: Selector
generateRandomSetupPasscodeSelector = mkSelector "generateRandomSetupPasscode"

-- | @Selector@ for @initWithSetupPasscode:discriminator:@
initWithSetupPasscode_discriminatorSelector :: Selector
initWithSetupPasscode_discriminatorSelector = mkSelector "initWithSetupPasscode:discriminator:"

-- | @Selector@ for @manualEntryCode@
manualEntryCodeSelector :: Selector
manualEntryCodeSelector = mkSelector "manualEntryCode"

-- | @Selector@ for @qrCodeString@
qrCodeStringSelector :: Selector
qrCodeStringSelector = mkSelector "qrCodeString"

-- | @Selector@ for @isValidSetupPasscode:@
isValidSetupPasscodeSelector :: Selector
isValidSetupPasscodeSelector = mkSelector "isValidSetupPasscode:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setupPayloadWithOnboardingPayload:error:@
setupPayloadWithOnboardingPayload_errorSelector :: Selector
setupPayloadWithOnboardingPayload_errorSelector = mkSelector "setupPayloadWithOnboardingPayload:error:"

-- | @Selector@ for @getAllOptionalVendorData:@
getAllOptionalVendorDataSelector :: Selector
getAllOptionalVendorDataSelector = mkSelector "getAllOptionalVendorData:"

-- | @Selector@ for @concatenated@
concatenatedSelector :: Selector
concatenatedSelector = mkSelector "concatenated"

-- | @Selector@ for @subPayloads@
subPayloadsSelector :: Selector
subPayloadsSelector = mkSelector "subPayloads"

-- | @Selector@ for @setSubPayloads:@
setSubPayloadsSelector :: Selector
setSubPayloadsSelector = mkSelector "setSubPayloads:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector
setProductIDSelector = mkSelector "setProductID:"

-- | @Selector@ for @commissioningFlow@
commissioningFlowSelector :: Selector
commissioningFlowSelector = mkSelector "commissioningFlow"

-- | @Selector@ for @setCommissioningFlow:@
setCommissioningFlowSelector :: Selector
setCommissioningFlowSelector = mkSelector "setCommissioningFlow:"

-- | @Selector@ for @discoveryCapabilities@
discoveryCapabilitiesSelector :: Selector
discoveryCapabilitiesSelector = mkSelector "discoveryCapabilities"

-- | @Selector@ for @setDiscoveryCapabilities:@
setDiscoveryCapabilitiesSelector :: Selector
setDiscoveryCapabilitiesSelector = mkSelector "setDiscoveryCapabilities:"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @setDiscriminator:@
setDiscriminatorSelector :: Selector
setDiscriminatorSelector = mkSelector "setDiscriminator:"

-- | @Selector@ for @hasShortDiscriminator@
hasShortDiscriminatorSelector :: Selector
hasShortDiscriminatorSelector = mkSelector "hasShortDiscriminator"

-- | @Selector@ for @setHasShortDiscriminator:@
setHasShortDiscriminatorSelector :: Selector
setHasShortDiscriminatorSelector = mkSelector "setHasShortDiscriminator:"

-- | @Selector@ for @setupPasscode@
setupPasscodeSelector :: Selector
setupPasscodeSelector = mkSelector "setupPasscode"

-- | @Selector@ for @setSetupPasscode:@
setSetupPasscodeSelector :: Selector
setSetupPasscodeSelector = mkSelector "setSetupPasscode:"

-- | @Selector@ for @serialNumber@
serialNumberSelector :: Selector
serialNumberSelector = mkSelector "serialNumber"

-- | @Selector@ for @setSerialNumber:@
setSerialNumberSelector :: Selector
setSerialNumberSelector = mkSelector "setSerialNumber:"

-- | @Selector@ for @vendorElements@
vendorElementsSelector :: Selector
vendorElementsSelector = mkSelector "vendorElements"

-- | @Selector@ for @rendezvousInformation@
rendezvousInformationSelector :: Selector
rendezvousInformationSelector = mkSelector "rendezvousInformation"

-- | @Selector@ for @setRendezvousInformation:@
setRendezvousInformationSelector :: Selector
setRendezvousInformationSelector = mkSelector "setRendezvousInformation:"

-- | @Selector@ for @setUpPINCode@
setUpPINCodeSelector :: Selector
setUpPINCodeSelector = mkSelector "setUpPINCode"

-- | @Selector@ for @setSetUpPINCode:@
setSetUpPINCodeSelector :: Selector
setSetUpPINCodeSelector = mkSelector "setSetUpPINCode:"

