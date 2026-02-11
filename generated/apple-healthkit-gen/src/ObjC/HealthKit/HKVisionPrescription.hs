{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVisionPrescription
--
-- HKSample subclass representing a vision prescription
--
-- Generated bindings for @HKVisionPrescription@.
module ObjC.HealthKit.HKVisionPrescription
  ( HKVisionPrescription
  , IsHKVisionPrescription(..)
  , prescriptionWithType_dateIssued_expirationDate_device_metadata
  , init_
  , new
  , prescriptionType
  , dateIssued
  , expirationDate
  , prescriptionWithType_dateIssued_expirationDate_device_metadataSelector
  , initSelector
  , newSelector
  , prescriptionTypeSelector
  , dateIssuedSelector
  , expirationDateSelector

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

-- | prescriptionWithType:dateIssued:expirationDate:device:metadata
--
-- @type@ — The prescription type
--
-- @dateIssued@ — The date the prescription was issued
--
-- @expirationDate@ — The date the prescription expires
--
-- @device@ — The device that generated the sample
--
-- @metadata@ — The metadata for the sample
--
-- ObjC selector: @+ prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadata :: (IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => HKVisionPrescriptionType -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKVisionPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadata type_ dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKVisionPrescription"
    withObjCPtr dateIssued $ \raw_dateIssued ->
      withObjCPtr expirationDate $ \raw_expirationDate ->
        withObjCPtr device $ \raw_device ->
          withObjCPtr metadata $ \raw_metadata ->
            sendClassMsg cls' (mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:") (retPtr retVoid) [argCULong (coerce type_), argPtr (castPtr raw_dateIssued :: Ptr ()), argPtr (castPtr raw_expirationDate :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id HKVisionPrescription)
init_ hkVisionPrescription  =
    sendMsg hkVisionPrescription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKVisionPrescription)
new  =
  do
    cls' <- getRequiredClass "HKVisionPrescription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | prescriptionType
--
-- A vision prescription type (glasses or contacts)
--
-- ObjC selector: @- prescriptionType@
prescriptionType :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO HKVisionPrescriptionType
prescriptionType hkVisionPrescription  =
    fmap (coerce :: CULong -> HKVisionPrescriptionType) $ sendMsg hkVisionPrescription (mkSelector "prescriptionType") retCULong []

-- | dateIssued
--
-- The date the prescription was issued
--
-- ObjC selector: @- dateIssued@
dateIssued :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id NSDate)
dateIssued hkVisionPrescription  =
    sendMsg hkVisionPrescription (mkSelector "dateIssued") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | expirationDate
--
-- The date the prescription will expire
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id NSDate)
expirationDate hkVisionPrescription  =
    sendMsg hkVisionPrescription (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector :: Selector
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @prescriptionType@
prescriptionTypeSelector :: Selector
prescriptionTypeSelector = mkSelector "prescriptionType"

-- | @Selector@ for @dateIssued@
dateIssuedSelector :: Selector
dateIssuedSelector = mkSelector "dateIssued"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

