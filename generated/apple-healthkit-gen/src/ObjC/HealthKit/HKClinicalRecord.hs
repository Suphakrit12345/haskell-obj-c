{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKClinicalRecord
--
-- An HKObject subclass representing a health record.
--
-- The startDate and endDate properties (inherited from HKSample) are set to the date the sample was               added to Health. Unlike other HKObject subclasses, UUID is not a stable identifier               for a given sample. Use a combination of HKSource, FHIRResource.resourceType, and               FHIRResource.identifier instead.
--
-- Generated bindings for @HKClinicalRecord@.
module ObjC.HealthKit.HKClinicalRecord
  ( HKClinicalRecord
  , IsHKClinicalRecord(..)
  , init_
  , new
  , clinicalType
  , displayName
  , fhirResource
  , initSelector
  , newSelector
  , clinicalTypeSelector
  , displayNameSelector
  , fhirResourceSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKClinicalRecord)
init_ hkClinicalRecord  =
    sendMsg hkClinicalRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKClinicalRecord)
new  =
  do
    cls' <- getRequiredClass "HKClinicalRecord"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- clinicalType@
clinicalType :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKClinicalType)
clinicalType hkClinicalRecord  =
    sendMsg hkClinicalRecord (mkSelector "clinicalType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | displayName
--
-- The primary display name used in Health.
--
-- The display name is not localized, and is generally expected to be US English.
--
-- ObjC selector: @- displayName@
displayName :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id NSString)
displayName hkClinicalRecord  =
    sendMsg hkClinicalRecord (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | FHIRResource
--
-- The FHIR resource (where applicable) backing this sample.
--
-- ObjC selector: @- FHIRResource@
fhirResource :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKFHIRResource)
fhirResource hkClinicalRecord  =
    sendMsg hkClinicalRecord (mkSelector "FHIRResource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @clinicalType@
clinicalTypeSelector :: Selector
clinicalTypeSelector = mkSelector "clinicalType"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @FHIRResource@
fhirResourceSelector :: Selector
fhirResourceSelector = mkSelector "FHIRResource"

