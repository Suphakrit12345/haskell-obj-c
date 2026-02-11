{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSourceRevision
--
-- Represents a specific revision of an HKSource.
--
-- Generated bindings for @HKSourceRevision@.
module ObjC.HealthKit.HKSourceRevision
  ( HKSourceRevision
  , IsHKSourceRevision(..)
  , initWithSource_version
  , init_
  , source
  , version
  , productType
  , initWithSource_versionSelector
  , initSelector
  , sourceSelector
  , versionSelector
  , productTypeSelector


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

-- | initWithSource:version:
--
-- Initializes a new HKSourceRevision with the given source and version.
--
-- ObjC selector: @- initWithSource:version:@
initWithSource_version :: (IsHKSourceRevision hkSourceRevision, IsHKSource source, IsNSString version) => hkSourceRevision -> source -> version -> IO (Id HKSourceRevision)
initWithSource_version hkSourceRevision  source version =
  withObjCPtr source $ \raw_source ->
    withObjCPtr version $ \raw_version ->
        sendMsg hkSourceRevision (mkSelector "initWithSource:version:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_version :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id HKSourceRevision)
init_ hkSourceRevision  =
    sendMsg hkSourceRevision (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | source
--
-- The HKSource of the receiver.
--
-- ObjC selector: @- source@
source :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id HKSource)
source hkSourceRevision  =
    sendMsg hkSourceRevision (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | version
--
-- The version of the source property.
--
-- This value is taken from the CFBundleVersion of the source. May be nil for older data.
--
-- ObjC selector: @- version@
version :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id NSString)
version hkSourceRevision  =
    sendMsg hkSourceRevision (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | productType
--
-- Represents the product type of the device running HealthKit when the object was created.
--
-- This value may be nil for older data, which indicates an unknown product type.
--
-- ObjC selector: @- productType@
productType :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id NSString)
productType hkSourceRevision  =
    sendMsg hkSourceRevision (mkSelector "productType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:version:@
initWithSource_versionSelector :: Selector
initWithSource_versionSelector = mkSelector "initWithSource:version:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @productType@
productTypeSelector :: Selector
productTypeSelector = mkSelector "productType"

