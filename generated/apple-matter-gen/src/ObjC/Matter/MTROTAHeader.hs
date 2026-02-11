{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTAHeader@.
module ObjC.Matter.MTROTAHeader
  ( MTROTAHeader
  , IsMTROTAHeader(..)
  , initWithData
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , payloadSize
  , setPayloadSize
  , softwareVersion
  , setSoftwareVersion
  , softwareVersionString
  , setSoftwareVersionString
  , releaseNotesURL
  , setReleaseNotesURL
  , imageDigest
  , setImageDigest
  , imageDigestType
  , setImageDigestType
  , minApplicableVersion
  , setMinApplicableVersion
  , maxApplicableVersion
  , setMaxApplicableVersion
  , initWithDataSelector
  , vendorIDSelector
  , setVendorIDSelector
  , productIDSelector
  , setProductIDSelector
  , payloadSizeSelector
  , setPayloadSizeSelector
  , softwareVersionSelector
  , setSoftwareVersionSelector
  , softwareVersionStringSelector
  , setSoftwareVersionStringSelector
  , releaseNotesURLSelector
  , setReleaseNotesURLSelector
  , imageDigestSelector
  , setImageDigestSelector
  , imageDigestTypeSelector
  , setImageDigestTypeSelector
  , minApplicableVersionSelector
  , setMinApplicableVersionSelector
  , maxApplicableVersionSelector
  , setMaxApplicableVersionSelector

  -- * Enum types
  , MTROTAImageDigestType(MTROTAImageDigestType)
  , pattern MTROTAImageDigestTypeSha256
  , pattern MTROTAImageDigestTypeSha256_128
  , pattern MTROTAImageDigestTypeSha256_120
  , pattern MTROTAImageDigestTypeSha256_96
  , pattern MTROTAImageDigestTypeSha256_64
  , pattern MTROTAImageDigestTypeSha256_32
  , pattern MTROTAImageDigestTypeSha384
  , pattern MTROTAImageDigestTypeSha512
  , pattern MTROTAImageDigestTypeSha3_224
  , pattern MTROTAImageDigestTypeSha3_256
  , pattern MTROTAImageDigestTypeSha3_384
  , pattern MTROTAImageDigestTypeSha3_512

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

-- | Initialize the MTROTAHeader with the given Matter OTA software image data (as defined in the "Over-the-Air (OTA) Software Update File Format" section of the Matter specification).  The provided data is expected to point to a large enough initial chunk of an OTA software image that it includes the entire header (e.g. the entire image).
--
-- If the passed-in data is too small and does not contain the entire OTA image header, initWithData will return nil and the caller should try creating a new MTROTAHeader object and initializing it with a larger chunk of the image.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsMTROTAHeader mtrotaHeader, IsNSData data_) => mtrotaHeader -> data_ -> IO (Id MTROTAHeader)
initWithData mtrotaHeader  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg mtrotaHeader (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | The identifier of the vendor whose product this image is meant for.
--
-- This field can be compared to the vendor id received in the Query Image command to determine whether an image matches.
--
-- This field may be 0, in which case the image might apply to products from more than one vendor.  If it's nonzero, it must match the vendor id in Query Image for this image to be considered.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
vendorID mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the vendor whose product this image is meant for.
--
-- This field can be compared to the vendor id received in the Query Image command to determine whether an image matches.
--
-- This field may be 0, in which case the image might apply to products from more than one vendor.  If it's nonzero, it must match the vendor id in Query Image for this image to be considered.
--
-- ObjC selector: @- setVendorID:@
setVendorID :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setVendorID mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The identifier of the specific product the image is meant for.  May be 0, if the image might apply to more than one product.  This is allowed, but not required, to be matched against the product id received in Query Image.
--
-- ObjC selector: @- productID@
productID :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
productID mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the specific product the image is meant for.  May be 0, if the image might apply to more than one product.  This is allowed, but not required, to be matched against the product id received in Query Image.
--
-- ObjC selector: @- setProductID:@
setProductID :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setProductID mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setProductID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The size of the actual image payload, which follows the header in the OTA file.
--
-- ObjC selector: @- payloadSize@
payloadSize :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
payloadSize mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "payloadSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The size of the actual image payload, which follows the header in the OTA file.
--
-- ObjC selector: @- setPayloadSize:@
setPayloadSize :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setPayloadSize mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setPayloadSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The version of the software contained in this image.  This is the version the OTA requestor will be updated to if this image is installed.  This can be used to determine whether this image is newer than what the requestor is currently running, by comparing it to the SoftwareVersion in the Query Image command.
--
-- ObjC selector: @- softwareVersion@
softwareVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
softwareVersion mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of the software contained in this image.  This is the version the OTA requestor will be updated to if this image is installed.  This can be used to determine whether this image is newer than what the requestor is currently running, by comparing it to the SoftwareVersion in the Query Image command.
--
-- ObjC selector: @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setSoftwareVersion mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Human-readable version of softwareVersion.  This must not be used for deciding which versions are newer or older; use softwareVersion for that.
--
-- ObjC selector: @- softwareVersionString@
softwareVersionString :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSString)
softwareVersionString mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "softwareVersionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Human-readable version of softwareVersion.  This must not be used for deciding which versions are newer or older; use softwareVersion for that.
--
-- ObjC selector: @- setSoftwareVersionString:@
setSoftwareVersionString :: (IsMTROTAHeader mtrotaHeader, IsNSString value) => mtrotaHeader -> value -> IO ()
setSoftwareVersionString mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setSoftwareVersionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If not nil a URL pointing to release notes for the software update represented by the image.
--
-- ObjC selector: @- releaseNotesURL@
releaseNotesURL :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSString)
releaseNotesURL mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "releaseNotesURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If not nil a URL pointing to release notes for the software update represented by the image.
--
-- ObjC selector: @- setReleaseNotesURL:@
setReleaseNotesURL :: (IsMTROTAHeader mtrotaHeader, IsNSString value) => mtrotaHeader -> value -> IO ()
setReleaseNotesURL mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setReleaseNotesURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A digest of the payload that follows the header.  Can be used to verify that the payload is not truncated or corrupted.
--
-- ObjC selector: @- imageDigest@
imageDigest :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSData)
imageDigest mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "imageDigest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A digest of the payload that follows the header.  Can be used to verify that the payload is not truncated or corrupted.
--
-- ObjC selector: @- setImageDigest:@
setImageDigest :: (IsMTROTAHeader mtrotaHeader, IsNSData value) => mtrotaHeader -> value -> IO ()
setImageDigest mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setImageDigest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The specific algorithm that was used to compute imageDigest.
--
-- ObjC selector: @- imageDigestType@
imageDigestType :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO MTROTAImageDigestType
imageDigestType mtrotaHeader  =
    fmap (coerce :: CULong -> MTROTAImageDigestType) $ sendMsg mtrotaHeader (mkSelector "imageDigestType") retCULong []

-- | The specific algorithm that was used to compute imageDigest.
--
-- ObjC selector: @- setImageDigestType:@
setImageDigestType :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> MTROTAImageDigestType -> IO ()
setImageDigestType mtrotaHeader  value =
    sendMsg mtrotaHeader (mkSelector "setImageDigestType:") retVoid [argCULong (coerce value)]

-- | If not nil, specifies the smallest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- minApplicableVersion@
minApplicableVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
minApplicableVersion mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "minApplicableVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If not nil, specifies the smallest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- setMinApplicableVersion:@
setMinApplicableVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setMinApplicableVersion mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setMinApplicableVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If not nil, specifies the largest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- maxApplicableVersion@
maxApplicableVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
maxApplicableVersion mtrotaHeader  =
    sendMsg mtrotaHeader (mkSelector "maxApplicableVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If not nil, specifies the largest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- setMaxApplicableVersion:@
setMaxApplicableVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setMaxApplicableVersion mtrotaHeader  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrotaHeader (mkSelector "setMaxApplicableVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

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

-- | @Selector@ for @payloadSize@
payloadSizeSelector :: Selector
payloadSizeSelector = mkSelector "payloadSize"

-- | @Selector@ for @setPayloadSize:@
setPayloadSizeSelector :: Selector
setPayloadSizeSelector = mkSelector "setPayloadSize:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @softwareVersionString@
softwareVersionStringSelector :: Selector
softwareVersionStringSelector = mkSelector "softwareVersionString"

-- | @Selector@ for @setSoftwareVersionString:@
setSoftwareVersionStringSelector :: Selector
setSoftwareVersionStringSelector = mkSelector "setSoftwareVersionString:"

-- | @Selector@ for @releaseNotesURL@
releaseNotesURLSelector :: Selector
releaseNotesURLSelector = mkSelector "releaseNotesURL"

-- | @Selector@ for @setReleaseNotesURL:@
setReleaseNotesURLSelector :: Selector
setReleaseNotesURLSelector = mkSelector "setReleaseNotesURL:"

-- | @Selector@ for @imageDigest@
imageDigestSelector :: Selector
imageDigestSelector = mkSelector "imageDigest"

-- | @Selector@ for @setImageDigest:@
setImageDigestSelector :: Selector
setImageDigestSelector = mkSelector "setImageDigest:"

-- | @Selector@ for @imageDigestType@
imageDigestTypeSelector :: Selector
imageDigestTypeSelector = mkSelector "imageDigestType"

-- | @Selector@ for @setImageDigestType:@
setImageDigestTypeSelector :: Selector
setImageDigestTypeSelector = mkSelector "setImageDigestType:"

-- | @Selector@ for @minApplicableVersion@
minApplicableVersionSelector :: Selector
minApplicableVersionSelector = mkSelector "minApplicableVersion"

-- | @Selector@ for @setMinApplicableVersion:@
setMinApplicableVersionSelector :: Selector
setMinApplicableVersionSelector = mkSelector "setMinApplicableVersion:"

-- | @Selector@ for @maxApplicableVersion@
maxApplicableVersionSelector :: Selector
maxApplicableVersionSelector = mkSelector "maxApplicableVersion"

-- | @Selector@ for @setMaxApplicableVersion:@
setMaxApplicableVersionSelector :: Selector
setMaxApplicableVersionSelector = mkSelector "setMaxApplicableVersion:"

