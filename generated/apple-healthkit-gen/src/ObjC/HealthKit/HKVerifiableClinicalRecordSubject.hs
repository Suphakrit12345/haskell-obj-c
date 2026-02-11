{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVerifiableClinicalRecordSubject
--
-- An NSObject that represents a verifiable clinical record subject.
--
-- Generated bindings for @HKVerifiableClinicalRecordSubject@.
module ObjC.HealthKit.HKVerifiableClinicalRecordSubject
  ( HKVerifiableClinicalRecordSubject
  , IsHKVerifiableClinicalRecordSubject(..)
  , init_
  , new
  , fullName
  , dateOfBirthComponents
  , initSelector
  , newSelector
  , fullNameSelector
  , dateOfBirthComponentsSelector


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
init_ :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id HKVerifiableClinicalRecordSubject)
init_ hkVerifiableClinicalRecordSubject  =
    sendMsg hkVerifiableClinicalRecordSubject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecordSubject)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecordSubject"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | fullName
--
-- The subject's full name.
--
-- ObjC selector: @- fullName@
fullName :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id NSString)
fullName hkVerifiableClinicalRecordSubject  =
    sendMsg hkVerifiableClinicalRecordSubject (mkSelector "fullName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dateOfBirthComponents
--
-- The subject's date of birth components.
--
-- ObjC selector: @- dateOfBirthComponents@
dateOfBirthComponents :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id NSDateComponents)
dateOfBirthComponents hkVerifiableClinicalRecordSubject  =
    sendMsg hkVerifiableClinicalRecordSubject (mkSelector "dateOfBirthComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @dateOfBirthComponents@
dateOfBirthComponentsSelector :: Selector
dateOfBirthComponentsSelector = mkSelector "dateOfBirthComponents"

