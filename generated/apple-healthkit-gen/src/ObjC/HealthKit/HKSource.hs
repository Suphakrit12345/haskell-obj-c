{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSource
--
-- Represents the entity that created an object stored by HealthKit.
--
-- Generated bindings for @HKSource@.
module ObjC.HealthKit.HKSource
  ( HKSource
  , IsHKSource(..)
  , defaultSource
  , init_
  , name
  , bundleIdentifier
  , defaultSourceSelector
  , initSelector
  , nameSelector
  , bundleIdentifierSelector


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

-- | defaultSource
--
-- Returns the source representing the calling application.
--
-- ObjC selector: @+ defaultSource@
defaultSource :: IO (Id HKSource)
defaultSource  =
  do
    cls' <- getRequiredClass "HKSource"
    sendClassMsg cls' (mkSelector "defaultSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKSource hkSource => hkSource -> IO (Id HKSource)
init_ hkSource  =
    sendMsg hkSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | name
--
-- The name of the source represented by the receiver.  If the source is an app, then the name is the                localized name of the app.
--
-- ObjC selector: @- name@
name :: IsHKSource hkSource => hkSource -> IO (Id NSString)
name hkSource  =
    sendMsg hkSource (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bundleIdentifier
--
-- The bundle identifier of the source represented by the receiver.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsHKSource hkSource => hkSource -> IO (Id NSString)
bundleIdentifier hkSource  =
    sendMsg hkSource (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSource@
defaultSourceSelector :: Selector
defaultSourceSelector = mkSelector "defaultSource"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

