{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKContactsLensSpecification
--
-- An object subclass representing lens specification for contacts
--
-- Generated bindings for @HKContactsLensSpecification@.
module ObjC.HealthKit.HKContactsLensSpecification
  ( HKContactsLensSpecification
  , IsHKContactsLensSpecification(..)
  , initWithSphere_cylinder_axis_addPower_baseCurve_diameter
  , init_
  , new
  , baseCurve
  , diameter
  , initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector
  , initSelector
  , newSelector
  , baseCurveSelector
  , diameterSelector


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

-- | initWithSphere:cylinder:axis:addPower:baseCurve:diameter
--
-- @sphere@ — The lens power to correct nearsightedness or farsightedness
--
-- @cylinder@ — The lens power required to correct astigmatism
--
-- @axis@ — The angle along which cylindrical power should be positioned to correct astigmatism
--
-- @addPower@ — The power adjustment applied to a multifocal lens to correct presbyopia
--
-- @baseCurve@ — The curvature of the back surface of the lens
--
-- @diameter@ — The width of the lens from edge to edge
--
-- ObjC selector: @- initWithSphere:cylinder:axis:addPower:baseCurve:diameter:@
initWithSphere_cylinder_axis_addPower_baseCurve_diameter :: (IsHKContactsLensSpecification hkContactsLensSpecification, IsHKQuantity sphere, IsHKQuantity cylinder, IsHKQuantity axis, IsHKQuantity addPower, IsHKQuantity baseCurve, IsHKQuantity diameter) => hkContactsLensSpecification -> sphere -> cylinder -> axis -> addPower -> baseCurve -> diameter -> IO (Id HKContactsLensSpecification)
initWithSphere_cylinder_axis_addPower_baseCurve_diameter hkContactsLensSpecification  sphere cylinder axis addPower baseCurve diameter =
  withObjCPtr sphere $ \raw_sphere ->
    withObjCPtr cylinder $ \raw_cylinder ->
      withObjCPtr axis $ \raw_axis ->
        withObjCPtr addPower $ \raw_addPower ->
          withObjCPtr baseCurve $ \raw_baseCurve ->
            withObjCPtr diameter $ \raw_diameter ->
                sendMsg hkContactsLensSpecification (mkSelector "initWithSphere:cylinder:axis:addPower:baseCurve:diameter:") (retPtr retVoid) [argPtr (castPtr raw_sphere :: Ptr ()), argPtr (castPtr raw_cylinder :: Ptr ()), argPtr (castPtr raw_axis :: Ptr ()), argPtr (castPtr raw_addPower :: Ptr ()), argPtr (castPtr raw_baseCurve :: Ptr ()), argPtr (castPtr raw_diameter :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKContactsLensSpecification)
init_ hkContactsLensSpecification  =
    sendMsg hkContactsLensSpecification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKContactsLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKContactsLensSpecification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | baseCurve
--
-- The curvature of the back surface of the lens (measured in mm)
--
-- ObjC selector: @- baseCurve@
baseCurve :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKQuantity)
baseCurve hkContactsLensSpecification  =
    sendMsg hkContactsLensSpecification (mkSelector "baseCurve") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | diameter
--
-- The width of the lens from edge to edge (measured in mm)
--
-- ObjC selector: @- diameter@
diameter :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKQuantity)
diameter hkContactsLensSpecification  =
    sendMsg hkContactsLensSpecification (mkSelector "diameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSphere:cylinder:axis:addPower:baseCurve:diameter:@
initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector :: Selector
initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector = mkSelector "initWithSphere:cylinder:axis:addPower:baseCurve:diameter:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @baseCurve@
baseCurveSelector :: Selector
baseCurveSelector = mkSelector "baseCurve"

-- | @Selector@ for @diameter@
diameterSelector :: Selector
diameterSelector = mkSelector "diameter"

