{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKGlassesLensSpecification
--
-- An object subclass representing lens specification for glasses
--
-- Generated bindings for @HKGlassesLensSpecification@.
module ObjC.HealthKit.HKGlassesLensSpecification
  ( HKGlassesLensSpecification
  , IsHKGlassesLensSpecification(..)
  , initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance
  , init_
  , new
  , vertexDistance
  , prism
  , farPupillaryDistance
  , nearPupillaryDistance
  , initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector
  , initSelector
  , newSelector
  , vertexDistanceSelector
  , prismSelector
  , farPupillaryDistanceSelector
  , nearPupillaryDistanceSelector


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

-- | initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance
--
-- @sphere@ — The lens power to correct nearsightedness or farsightedness
--
-- @cylinder@ — The lens power required to correct astigmatism
--
-- @axis@ — The angle along which cylindrical power should be positioned to correct astigmatism
--
-- @addPower@ — The power adjustment applied to a multifocal lens to correct presbyopia
--
-- @vertexDistance@ — The distance between the back of the eyeglass lens and the eye
--
-- @prism@ — The object encapsulating the prism fields
--
-- @farPupillaryDistance@ — The distance from each pupil to the center of the nose (measured in mm) when looking at a far target.                                        Can be described as combined or individual value. For distance prescriptions, the pupillary distance will be a far value.
--
-- @nearPupillaryDistance@ — The distance from each pupil to the center of the nose (measured in mm) when looking at a near target.                                        Can be described as combined or individual value. For near prescriptions, the pupillary distance will be a near value.
--
-- ObjC selector: @- initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:@
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance :: (IsHKGlassesLensSpecification hkGlassesLensSpecification, IsHKQuantity sphere, IsHKQuantity cylinder, IsHKQuantity axis, IsHKQuantity addPower, IsHKQuantity vertexDistance, IsHKVisionPrism prism, IsHKQuantity farPupillaryDistance, IsHKQuantity nearPupillaryDistance) => hkGlassesLensSpecification -> sphere -> cylinder -> axis -> addPower -> vertexDistance -> prism -> farPupillaryDistance -> nearPupillaryDistance -> IO (Id HKGlassesLensSpecification)
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance hkGlassesLensSpecification  sphere cylinder axis addPower vertexDistance prism farPupillaryDistance nearPupillaryDistance =
  withObjCPtr sphere $ \raw_sphere ->
    withObjCPtr cylinder $ \raw_cylinder ->
      withObjCPtr axis $ \raw_axis ->
        withObjCPtr addPower $ \raw_addPower ->
          withObjCPtr vertexDistance $ \raw_vertexDistance ->
            withObjCPtr prism $ \raw_prism ->
              withObjCPtr farPupillaryDistance $ \raw_farPupillaryDistance ->
                withObjCPtr nearPupillaryDistance $ \raw_nearPupillaryDistance ->
                    sendMsg hkGlassesLensSpecification (mkSelector "initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:") (retPtr retVoid) [argPtr (castPtr raw_sphere :: Ptr ()), argPtr (castPtr raw_cylinder :: Ptr ()), argPtr (castPtr raw_axis :: Ptr ()), argPtr (castPtr raw_addPower :: Ptr ()), argPtr (castPtr raw_vertexDistance :: Ptr ()), argPtr (castPtr raw_prism :: Ptr ()), argPtr (castPtr raw_farPupillaryDistance :: Ptr ()), argPtr (castPtr raw_nearPupillaryDistance :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKGlassesLensSpecification)
init_ hkGlassesLensSpecification  =
    sendMsg hkGlassesLensSpecification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKGlassesLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKGlassesLensSpecification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | vertexDistance
--
-- The distance between the back of the eyeglass lens and the eye (measured in mm)
--
-- ObjC selector: @- vertexDistance@
vertexDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
vertexDistance hkGlassesLensSpecification  =
    sendMsg hkGlassesLensSpecification (mkSelector "vertexDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | prism
--
-- The object encapsulating the prism fields
--
-- ObjC selector: @- prism@
prism :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKVisionPrism)
prism hkGlassesLensSpecification  =
    sendMsg hkGlassesLensSpecification (mkSelector "prism") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | farPupillaryDistance
--
-- The distance from each pupil to the center of the nose (measured in mm) when looking at a far target.                Can be described as combined or individual value. For distance prescriptions, the pupillary distance will be a far value.
--
-- ObjC selector: @- farPupillaryDistance@
farPupillaryDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
farPupillaryDistance hkGlassesLensSpecification  =
    sendMsg hkGlassesLensSpecification (mkSelector "farPupillaryDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nearPupillaryDistance
--
-- The distance from each pupil to the center of the nose (measured in mm) when looking at a near target.                Can be described as combined or individual value. For near prescriptions, the pupillary distance will be a near value.
--
-- ObjC selector: @- nearPupillaryDistance@
nearPupillaryDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
nearPupillaryDistance hkGlassesLensSpecification  =
    sendMsg hkGlassesLensSpecification (mkSelector "nearPupillaryDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:@
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector :: Selector
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector = mkSelector "initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @vertexDistance@
vertexDistanceSelector :: Selector
vertexDistanceSelector = mkSelector "vertexDistance"

-- | @Selector@ for @prism@
prismSelector :: Selector
prismSelector = mkSelector "prism"

-- | @Selector@ for @farPupillaryDistance@
farPupillaryDistanceSelector :: Selector
farPupillaryDistanceSelector = mkSelector "farPupillaryDistance"

-- | @Selector@ for @nearPupillaryDistance@
nearPupillaryDistanceSelector :: Selector
nearPupillaryDistanceSelector = mkSelector "nearPupillaryDistance"

