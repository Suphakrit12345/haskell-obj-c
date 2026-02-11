{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKGlassesPrescription
--
-- An object subclass representing a glasses prescription
--
-- Generated bindings for @HKGlassesPrescription@.
module ObjC.HealthKit.HKGlassesPrescription
  ( HKGlassesPrescription
  , IsHKGlassesPrescription(..)
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata
  , init_
  , new
  , prescriptionWithType_dateIssued_expirationDate_device_metadata
  , rightEye
  , leftEye
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector
  , initSelector
  , newSelector
  , prescriptionWithType_dateIssued_expirationDate_device_metadataSelector
  , rightEyeSelector
  , leftEyeSelector

  -- * Enum types
  , HKVisionPrescriptionType(HKVisionPrescriptionType)
  , pattern HKVisionPrescriptionTypeGlasses
  , pattern HKVisionPrescriptionTypeContacts

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | prescriptionWithRightEyeSpecification:leftEyeSpecification:type:dateIssued:expirationDate:device:metadata
--
-- @rightEyeSpecification@ — The right eye specification
--
-- @leftEyeSpecification@ — The left eye specification
--
-- @dateIssued@ — The date the prescription was issued
--
-- @expirationDate@ — The date the prescription expires
--
-- @device@ — The device that generated the sample
--
-- @metadata@ — The metadata for the sample
--
-- ObjC selector: @+ prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata :: (IsHKGlassesLensSpecification rightEyeSpecification, IsHKGlassesLensSpecification leftEyeSpecification, IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => rightEyeSpecification -> leftEyeSpecification -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKGlassesPrescription)
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata rightEyeSpecification leftEyeSpecification dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    withObjCPtr rightEyeSpecification $ \raw_rightEyeSpecification ->
      withObjCPtr leftEyeSpecification $ \raw_leftEyeSpecification ->
        withObjCPtr dateIssued $ \raw_dateIssued ->
          withObjCPtr expirationDate $ \raw_expirationDate ->
            withObjCPtr device $ \raw_device ->
              withObjCPtr metadata $ \raw_metadata ->
                sendClassMsg cls' (mkSelector "prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:") (retPtr retVoid) [argPtr (castPtr raw_rightEyeSpecification :: Ptr ()), argPtr (castPtr raw_leftEyeSpecification :: Ptr ()), argPtr (castPtr raw_dateIssued :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesPrescription)
init_ hkGlassesPrescription  =
    sendMsg hkGlassesPrescription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKGlassesPrescription)
new  =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadata :: (IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => HKVisionPrescriptionType -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKGlassesPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadata type_ dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    withObjCPtr dateIssued $ \raw_dateIssued ->
      withObjCPtr expirationDate $ \raw_expirationDate ->
        withObjCPtr device $ \raw_device ->
          withObjCPtr metadata $ \raw_metadata ->
            sendClassMsg cls' (mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:") (retPtr retVoid) [argCULong (coerce type_), argPtr (castPtr raw_dateIssued :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | rightEye
--
-- The right eye lens specification
--
-- ObjC selector: @- rightEye@
rightEye :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesLensSpecification)
rightEye hkGlassesPrescription  =
    sendMsg hkGlassesPrescription (mkSelector "rightEye") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leftEye
--
-- The left eye lens specification
--
-- ObjC selector: @- leftEye@
leftEye :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesLensSpecification)
leftEye hkGlassesPrescription  =
    sendMsg hkGlassesPrescription (mkSelector "leftEye") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector :: Selector
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector :: Selector
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @rightEye@
rightEyeSelector :: Selector
rightEyeSelector = mkSelector "rightEye"

-- | @Selector@ for @leftEye@
leftEyeSelector :: Selector
leftEyeSelector = mkSelector "leftEye"

