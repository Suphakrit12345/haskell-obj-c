{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKLensSpecification
--
-- An object subclass representing common lens specification
--
-- Generated bindings for @HKLensSpecification@.
module ObjC.HealthKit.HKLensSpecification
  ( HKLensSpecification
  , IsHKLensSpecification(..)
  , init_
  , new
  , sphere
  , cylinder
  , axis
  , addPower
  , initSelector
  , newSelector
  , sphereSelector
  , cylinderSelector
  , axisSelector
  , addPowerSelector


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
init_ :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKLensSpecification)
init_ hkLensSpecification  =
    sendMsg hkLensSpecification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKLensSpecification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | sphere
--
-- The lens power to correct nearsightedness or farsightedness. (-) means nearsighted while (+) farsighted.
--
-- ObjC selector: @- sphere@
sphere :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
sphere hkLensSpecification  =
    sendMsg hkLensSpecification (mkSelector "sphere") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cylinder
--
-- The lens power required to correct astigmatism. Can be positive or negative.
--
-- ObjC selector: @- cylinder@
cylinder :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
cylinder hkLensSpecification  =
    sendMsg hkLensSpecification (mkSelector "cylinder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | axis
--
-- The angle along which cylindrical power should be positioned to correct astigmatism
--
-- ObjC selector: @- axis@
axis :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
axis hkLensSpecification  =
    sendMsg hkLensSpecification (mkSelector "axis") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | addPower
--
-- The power adjustment applied to a multifocal lens to correct presbyopia
--
-- ObjC selector: @- addPower@
addPower :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
addPower hkLensSpecification  =
    sendMsg hkLensSpecification (mkSelector "addPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sphere@
sphereSelector :: Selector
sphereSelector = mkSelector "sphere"

-- | @Selector@ for @cylinder@
cylinderSelector :: Selector
cylinderSelector = mkSelector "cylinder"

-- | @Selector@ for @axis@
axisSelector :: Selector
axisSelector = mkSelector "axis"

-- | @Selector@ for @addPower@
addPowerSelector :: Selector
addPowerSelector = mkSelector "addPower"

