{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKFHIRVersion
--
-- Represents a FHIR version.
--
-- FHIR uses semantic versions ("1.0.2", "4.0.1") to communicate which FHIR version a server supports or a                given resource is represented in. A FHIR version is associated with one FHIR release.
--
-- See: http://hl7.org/fhir/versions.html#versions
--
-- Generated bindings for @HKFHIRVersion@.
module ObjC.HealthKit.HKFHIRVersion
  ( HKFHIRVersion
  , IsHKFHIRVersion(..)
  , init_
  , versionFromVersionString_error
  , primaryDSTU2Version
  , primaryR4Version
  , majorVersion
  , minorVersion
  , patchVersion
  , fhirRelease
  , stringRepresentation
  , initSelector
  , versionFromVersionString_errorSelector
  , primaryDSTU2VersionSelector
  , primaryR4VersionSelector
  , majorVersionSelector
  , minorVersionSelector
  , patchVersionSelector
  , fhirReleaseSelector
  , stringRepresentationSelector


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
init_ :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id HKFHIRVersion)
init_ hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ versionFromVersionString:error:@
versionFromVersionString_error :: (IsNSString versionString, IsNSError errorOut) => versionString -> errorOut -> IO (Id HKFHIRVersion)
versionFromVersionString_error versionString errorOut =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    withObjCPtr versionString $ \raw_versionString ->
      withObjCPtr errorOut $ \raw_errorOut ->
        sendClassMsg cls' (mkSelector "versionFromVersionString:error:") (retPtr retVoid) [argPtr (castPtr raw_versionString :: Ptr ()), argPtr (castPtr raw_errorOut :: Ptr ())] >>= retainedObject . castPtr

-- | @+ primaryDSTU2Version@
primaryDSTU2Version :: IO (Id HKFHIRVersion)
primaryDSTU2Version  =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    sendClassMsg cls' (mkSelector "primaryDSTU2Version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ primaryR4Version@
primaryR4Version :: IO (Id HKFHIRVersion)
primaryR4Version  =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    sendClassMsg cls' (mkSelector "primaryR4Version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- majorVersion@
majorVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
majorVersion hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "majorVersion") retCLong []

-- | @- minorVersion@
minorVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
minorVersion hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "minorVersion") retCLong []

-- | @- patchVersion@
patchVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
patchVersion hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "patchVersion") retCLong []

-- | @- FHIRRelease@
fhirRelease :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id NSString)
fhirRelease hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "FHIRRelease") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | stringRepresentation
--
-- A string representation in the format "{major}.{minor}.{patch}".
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id NSString)
stringRepresentation hkfhirVersion  =
    sendMsg hkfhirVersion (mkSelector "stringRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @versionFromVersionString:error:@
versionFromVersionString_errorSelector :: Selector
versionFromVersionString_errorSelector = mkSelector "versionFromVersionString:error:"

-- | @Selector@ for @primaryDSTU2Version@
primaryDSTU2VersionSelector :: Selector
primaryDSTU2VersionSelector = mkSelector "primaryDSTU2Version"

-- | @Selector@ for @primaryR4Version@
primaryR4VersionSelector :: Selector
primaryR4VersionSelector = mkSelector "primaryR4Version"

-- | @Selector@ for @majorVersion@
majorVersionSelector :: Selector
majorVersionSelector = mkSelector "majorVersion"

-- | @Selector@ for @minorVersion@
minorVersionSelector :: Selector
minorVersionSelector = mkSelector "minorVersion"

-- | @Selector@ for @patchVersion@
patchVersionSelector :: Selector
patchVersionSelector = mkSelector "patchVersion"

-- | @Selector@ for @FHIRRelease@
fhirReleaseSelector :: Selector
fhirReleaseSelector = mkSelector "FHIRRelease"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector
stringRepresentationSelector = mkSelector "stringRepresentation"

