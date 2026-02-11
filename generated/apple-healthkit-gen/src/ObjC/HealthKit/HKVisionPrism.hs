{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVisionPrism
--
-- An object subclass representing prism vision fields used in eye glasses to correct double vision.                The prism aligns the two images so only one is seen.
--
-- Generated bindings for @HKVisionPrism@.
module ObjC.HealthKit.HKVisionPrism
  ( HKVisionPrism
  , IsHKVisionPrism(..)
  , initWithAmount_angle_eye
  , initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye
  , init_
  , new
  , amount
  , angle
  , verticalAmount
  , horizontalAmount
  , verticalBase
  , horizontalBase
  , eye
  , initWithAmount_angle_eyeSelector
  , initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector
  , initSelector
  , newSelector
  , amountSelector
  , angleSelector
  , verticalAmountSelector
  , horizontalAmountSelector
  , verticalBaseSelector
  , horizontalBaseSelector
  , eyeSelector

  -- * Enum types
  , HKPrismBase(HKPrismBase)
  , pattern HKPrismBaseNone
  , pattern HKPrismBaseUp
  , pattern HKPrismBaseDown
  , pattern HKPrismBaseIn
  , pattern HKPrismBaseOut
  , HKVisionEye(HKVisionEye)
  , pattern HKVisionEyeLeft
  , pattern HKVisionEyeRight

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
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithAmount:angle:eye
--
-- @amount@ — The compensation for amount eye misalignment
--
-- @angle@ — The angle of the lens required to correct diplopia
--
-- @eye@ — The eye associated with the prism values
--
-- ObjC selector: @- initWithAmount:angle:eye:@
initWithAmount_angle_eye :: (IsHKVisionPrism hkVisionPrism, IsHKQuantity amount, IsHKQuantity angle) => hkVisionPrism -> amount -> angle -> HKVisionEye -> IO (Id HKVisionPrism)
initWithAmount_angle_eye hkVisionPrism  amount angle eye =
  withObjCPtr amount $ \raw_amount ->
    withObjCPtr angle $ \raw_angle ->
        sendMsg hkVisionPrism (mkSelector "initWithAmount:angle:eye:") (retPtr retVoid) [argPtr (castPtr raw_amount :: Ptr ()), argPtr (castPtr raw_angle :: Ptr ()), argCLong (coerce eye)] >>= ownedObject . castPtr

-- | initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye
--
-- @verticalAmount@ — The vertical component of compensation in prism diopters
--
-- @verticalBase@ — The direction of the prism base relative to the vertical axis of the lens;                                    base up or base down.
--
-- @horizontalAmount@ — The horizontal component of compensation in prism diopters
--
-- @horizontalBase@ — The direction of the prism base relative to the horizontal axis of the lens;                                    base in (toward the nose) or base out (away from the nose).
--
-- @eye@ — The eye associated with the prism values
--
-- ObjC selector: @- initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:@
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye :: (IsHKVisionPrism hkVisionPrism, IsHKQuantity verticalAmount, IsHKQuantity horizontalAmount) => hkVisionPrism -> verticalAmount -> HKPrismBase -> horizontalAmount -> HKPrismBase -> HKVisionEye -> IO (Id HKVisionPrism)
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eye hkVisionPrism  verticalAmount verticalBase horizontalAmount horizontalBase eye =
  withObjCPtr verticalAmount $ \raw_verticalAmount ->
    withObjCPtr horizontalAmount $ \raw_horizontalAmount ->
        sendMsg hkVisionPrism (mkSelector "initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:") (retPtr retVoid) [argPtr (castPtr raw_verticalAmount :: Ptr ()), argCLong (coerce verticalBase), argPtr (castPtr raw_horizontalAmount :: Ptr ()), argCLong (coerce horizontalBase), argCLong (coerce eye)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKVisionPrism)
init_ hkVisionPrism  =
    sendMsg hkVisionPrism (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKVisionPrism)
new  =
  do
    cls' <- getRequiredClass "HKVisionPrism"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | amount
--
-- The compensation in prism diopters to correct eye misalignment [polar coordinates]
--
-- ObjC selector: @- amount@
amount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
amount hkVisionPrism  =
    sendMsg hkVisionPrism (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | angle
--
-- The direction of the prism base [polar coordinates]
--
-- ObjC selector: @- angle@
angle :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
angle hkVisionPrism  =
    sendMsg hkVisionPrism (mkSelector "angle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | verticalAmount
--
-- The vertical component of compensation in prism diopters [rectangular coordinates]
--
-- ObjC selector: @- verticalAmount@
verticalAmount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
verticalAmount hkVisionPrism  =
    sendMsg hkVisionPrism (mkSelector "verticalAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | horizontalAmount
--
-- The horizontal component of compensation in prism diopters [rectangular coordinates]
--
-- ObjC selector: @- horizontalAmount@
horizontalAmount :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO (Id HKQuantity)
horizontalAmount hkVisionPrism  =
    sendMsg hkVisionPrism (mkSelector "horizontalAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | verticalBase
--
-- The direction of the prism base relative to the vertical axis of the lens;                base up or base down. [rectangular coordinates]
--
-- ObjC selector: @- verticalBase@
verticalBase :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKPrismBase
verticalBase hkVisionPrism  =
    fmap (coerce :: CLong -> HKPrismBase) $ sendMsg hkVisionPrism (mkSelector "verticalBase") retCLong []

-- | horizontalBase
--
-- The direction of the prism base relative to the horizontal axis of the lens;                base in (toward the nose) or base out (away from the nose). [rectangular coordinates]
--
-- ObjC selector: @- horizontalBase@
horizontalBase :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKPrismBase
horizontalBase hkVisionPrism  =
    fmap (coerce :: CLong -> HKPrismBase) $ sendMsg hkVisionPrism (mkSelector "horizontalBase") retCLong []

-- | eye
--
-- Which eye (left or right)
--
-- ObjC selector: @- eye@
eye :: IsHKVisionPrism hkVisionPrism => hkVisionPrism -> IO HKVisionEye
eye hkVisionPrism  =
    fmap (coerce :: CLong -> HKVisionEye) $ sendMsg hkVisionPrism (mkSelector "eye") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAmount:angle:eye:@
initWithAmount_angle_eyeSelector :: Selector
initWithAmount_angle_eyeSelector = mkSelector "initWithAmount:angle:eye:"

-- | @Selector@ for @initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:@
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector :: Selector
initWithVerticalAmount_verticalBase_horizontalAmount_horizontalBase_eyeSelector = mkSelector "initWithVerticalAmount:verticalBase:horizontalAmount:horizontalBase:eye:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @angle@
angleSelector :: Selector
angleSelector = mkSelector "angle"

-- | @Selector@ for @verticalAmount@
verticalAmountSelector :: Selector
verticalAmountSelector = mkSelector "verticalAmount"

-- | @Selector@ for @horizontalAmount@
horizontalAmountSelector :: Selector
horizontalAmountSelector = mkSelector "horizontalAmount"

-- | @Selector@ for @verticalBase@
verticalBaseSelector :: Selector
verticalBaseSelector = mkSelector "verticalBase"

-- | @Selector@ for @horizontalBase@
horizontalBaseSelector :: Selector
horizontalBaseSelector = mkSelector "horizontalBase"

-- | @Selector@ for @eye@
eyeSelector :: Selector
eyeSelector = mkSelector "eye"

