{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A reference to the tracked medication and the details a person can customize.
--
-- The details are relevant to the medication tracking experience.
--
-- Generated bindings for @HKUserAnnotatedMedication@.
module ObjC.HealthKit.HKUserAnnotatedMedication
  ( HKUserAnnotatedMedication
  , IsHKUserAnnotatedMedication(..)
  , init_
  , nickname
  , isArchived
  , hasSchedule
  , medication
  , initSelector
  , nicknameSelector
  , isArchivedSelector
  , hasScheduleSelector
  , medicationSelector


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
init_ :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id HKUserAnnotatedMedication)
init_ hkUserAnnotatedMedication  =
    sendMsg hkUserAnnotatedMedication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The nickname that a person added to a medication during the entry experience.
--
-- This can be edited at any point.
--
-- ObjC selector: @- nickname@
nickname :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id NSString)
nickname hkUserAnnotatedMedication  =
    sendMsg hkUserAnnotatedMedication (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether a medication is archived.
--
-- The value is @true@ if a person moves a medication to the archived section in the Health App. The value is @false@ if a medication isn't in the archived section.
--
-- ObjC selector: @- isArchived@
isArchived :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO Bool
isArchived hkUserAnnotatedMedication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkUserAnnotatedMedication (mkSelector "isArchived") retCULong []

-- | A Boolean value that indicates whether a medication has a schedule set up.
--
-- The value is @true@ for medications for which a person has set up reminders and @false@ for medications that are only taken as needed. > Note: Scheduled medications can still be taken as needed.
--
-- ObjC selector: @- hasSchedule@
hasSchedule :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO Bool
hasSchedule hkUserAnnotatedMedication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkUserAnnotatedMedication (mkSelector "hasSchedule") retCULong []

-- | A reference to the specific medication a person is tracking.
--
-- This concept's identifier is directly associated with the logged dose events.
--
-- ObjC selector: @- medication@
medication :: IsHKUserAnnotatedMedication hkUserAnnotatedMedication => hkUserAnnotatedMedication -> IO (Id HKMedicationConcept)
medication hkUserAnnotatedMedication  =
    sendMsg hkUserAnnotatedMedication (mkSelector "medication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @isArchived@
isArchivedSelector :: Selector
isArchivedSelector = mkSelector "isArchived"

-- | @Selector@ for @hasSchedule@
hasScheduleSelector :: Selector
hasScheduleSelector = mkSelector "hasSchedule"

-- | @Selector@ for @medication@
medicationSelector :: Selector
medicationSelector = mkSelector "medication"

