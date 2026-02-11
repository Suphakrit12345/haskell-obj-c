{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A clinical coding that represents a medical concept using a standardized coding system.
--
-- A clinical coding pairs a ``system``, an optional ``version``, and a ``code`` which identify a medical concept.
--
-- This model is closely related to the [FHIR Coding model](https://build.fhir.org/datatypes.html#Coding).
--
-- Generated bindings for @HKClinicalCoding@.
module ObjC.HealthKit.HKClinicalCoding
  ( HKClinicalCoding
  , IsHKClinicalCoding(..)
  , init_
  , initWithSystem_version_code
  , system
  , version
  , code
  , initSelector
  , initWithSystem_version_codeSelector
  , systemSelector
  , versionSelector
  , codeSelector


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
init_ :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id HKClinicalCoding)
init_ hkClinicalCoding  =
    sendMsg hkClinicalCoding (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a clinical coding with the specified system, version, and code.
--
-- @system@ — The string that identifies the coding system, typically a HL7 URL.
--
-- @version@ — The version of the system, if applicable.
--
-- @code@ — The clinical code string that represents the medical concept.
--
-- Use when you need to explicitly construct a coding object to associate a HealthKit concept with a standardized medical code.
--
-- ObjC selector: @- initWithSystem:version:code:@
initWithSystem_version_code :: (IsHKClinicalCoding hkClinicalCoding, IsNSString system, IsNSString version, IsNSString code) => hkClinicalCoding -> system -> version -> code -> IO (Id HKClinicalCoding)
initWithSystem_version_code hkClinicalCoding  system version code =
  withObjCPtr system $ \raw_system ->
    withObjCPtr version $ \raw_version ->
      withObjCPtr code $ \raw_code ->
          sendMsg hkClinicalCoding (mkSelector "initWithSystem:version:code:") (retPtr retVoid) [argPtr (castPtr raw_system :: Ptr ()), argPtr (castPtr raw_version :: Ptr ()), argPtr (castPtr raw_code :: Ptr ())] >>= ownedObject . castPtr

-- | The string that identifies the coding system that defines this clinical code.
--
-- The system is usually expressed as a URL from the [HL7 Terminology](https://terminology.hl7.org/). For example, the RxNorm, a coding system for medications uses: @http://www.nlm.nih.gov/research/umls/rxnorm@.
--
-- ObjC selector: @- system@
system :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
system hkClinicalCoding  =
    sendMsg hkClinicalCoding (mkSelector "system") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of the coding system.
--
-- ObjC selector: @- version@
version :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
version hkClinicalCoding  =
    sendMsg hkClinicalCoding (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The clinical code that represents a medical concept inside the coding system.
--
-- The format depends on the coding system. For example, RxNorm codes are numeric.
--
-- ObjC selector: @- code@
code :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
code hkClinicalCoding  =
    sendMsg hkClinicalCoding (mkSelector "code") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSystem:version:code:@
initWithSystem_version_codeSelector :: Selector
initWithSystem_version_codeSelector = mkSelector "initWithSystem:version:code:"

-- | @Selector@ for @system@
systemSelector :: Selector
systemSelector = mkSelector "system"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

