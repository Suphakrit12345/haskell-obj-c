{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that describes a specific medication concept.
--
-- A medication concept represents the idea of a medication, like ibuprofen or insulin. It can have clinical significance, or can be created by the person using your app.
--
-- Generated bindings for @HKMedicationConcept@.
module ObjC.HealthKit.HKMedicationConcept
  ( HKMedicationConcept
  , IsHKMedicationConcept(..)
  , init_
  , identifier
  , displayText
  , generalForm
  , relatedCodings
  , initSelector
  , identifierSelector
  , displayTextSelector
  , generalFormSelector
  , relatedCodingsSelector


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
init_ :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id HKMedicationConcept)
init_ hkMedicationConcept  =
    sendMsg hkMedicationConcept (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The unique identifier for the specific medication concept.
--
-- Each concept has one stable identifier that stays the same across devices. You can use this identifier to directly compare medications, for example, to check whether two objects represent the same medication.
--
-- ObjC selector: @- identifier@
identifier :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id HKHealthConceptIdentifier)
identifier hkMedicationConcept  =
    sendMsg hkMedicationConcept (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The display name for this medication.
--
-- The name of the medication a person enters or selects during medication onboarding.
--
-- ObjC selector: @- displayText@
displayText :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSString)
displayText hkMedicationConcept  =
    sendMsg hkMedicationConcept (mkSelector "displayText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The general form the medication is manufactured in.
--
-- A general manufactured dose form for the specific medication. This value tells you the manufactured form of the medication, such as tablet, capsule, cream, injection, or inhaler.
--
-- ObjC selector: @- generalForm@
generalForm :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSString)
generalForm hkMedicationConcept  =
    sendMsg hkMedicationConcept (mkSelector "generalForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The set of related clinical codings for the medication.
--
-- Each coding links the medication to an external medical terminology system, such as RxNorm.
--
-- ObjC selector: @- relatedCodings@
relatedCodings :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSSet)
relatedCodings hkMedicationConcept  =
    sendMsg hkMedicationConcept (mkSelector "relatedCodings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @displayText@
displayTextSelector :: Selector
displayTextSelector = mkSelector "displayText"

-- | @Selector@ for @generalForm@
generalFormSelector :: Selector
generalFormSelector = mkSelector "generalForm"

-- | @Selector@ for @relatedCodings@
relatedCodingsSelector :: Selector
relatedCodingsSelector = mkSelector "relatedCodings"

