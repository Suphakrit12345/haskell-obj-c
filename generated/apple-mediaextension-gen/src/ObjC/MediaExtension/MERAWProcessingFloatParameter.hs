{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingFloatParameter@.
module ObjC.MediaExtension.MERAWProcessingFloatParameter
  ( MERAWProcessingFloatParameter
  , IsMERAWProcessingFloatParameter(..)
  , initWithName_key_description_initialValue_maximum_minimum
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue
  , initWithName_key_description_initialValue_maximum_minimum_cameraValue
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , maximumValue
  , minimumValue
  , initialValue
  , currentValue
  , setCurrentValue
  , initWithName_key_description_initialValue_maximum_minimumSelector
  , initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector
  , initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector
  , hasNeutralValueSelector
  , hasCameraValueSelector
  , maximumValueSelector
  , minimumValueSelector
  , initialValueSelector
  , currentValueSelector
  , setCurrentValueSelector


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

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:key:description:initialValue:maximum:minimum:@
initWithName_key_description_initialValue_maximum_minimum :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum merawProcessingFloatParameter  name key description initialValue maximum_ minimum_ =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingFloatParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCFloat initialValue, argCFloat maximum_, argCFloat minimum_] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue merawProcessingFloatParameter  name key description initialValue maximum_ minimum_ neutralValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingFloatParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCFloat initialValue, argCFloat maximum_, argCFloat minimum_, argCFloat neutralValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValue merawProcessingFloatParameter  name key description initialValue maximum_ minimum_ cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingFloatParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCFloat initialValue, argCFloat maximum_, argCFloat minimum_, argCFloat cameraValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue merawProcessingFloatParameter  name key description initialValue maximum_ minimum_ neutralValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingFloatParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCFloat initialValue, argCFloat maximum_, argCFloat minimum_, argCFloat neutralValue, argCFloat cameraValue] >>= ownedObject . castPtr

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingFloatParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> Ptr CFloat -> IO Bool
hasNeutralValue merawProcessingFloatParameter  outNeutralValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingFloatParameter (mkSelector "hasNeutralValue:") retCULong [argPtr outNeutralValue]

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingFloatParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> Ptr CFloat -> IO Bool
hasCameraValue merawProcessingFloatParameter  outCameraValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingFloatParameter (mkSelector "hasCameraValue:") retCULong [argPtr outCameraValue]

-- | maximumValue
--
-- The maximum value for this parameter.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
maximumValue merawProcessingFloatParameter  =
    sendMsg merawProcessingFloatParameter (mkSelector "maximumValue") retCFloat []

-- | minimumValue
--
-- The minimum value for this parameter.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
minimumValue merawProcessingFloatParameter  =
    sendMsg merawProcessingFloatParameter (mkSelector "minimumValue") retCFloat []

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
initialValue merawProcessingFloatParameter  =
    sendMsg merawProcessingFloatParameter (mkSelector "initialValue") retCFloat []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
currentValue merawProcessingFloatParameter  =
    sendMsg merawProcessingFloatParameter (mkSelector "currentValue") retCFloat []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> CFloat -> IO ()
setCurrentValue merawProcessingFloatParameter  value =
    sendMsg merawProcessingFloatParameter (mkSelector "setCurrentValue:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:@
initWithName_key_description_initialValue_maximum_minimumSelector :: Selector
initWithName_key_description_initialValue_maximum_minimumSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector :: Selector
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector :: Selector
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:cameraValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector :: Selector
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @minimumValue@
minimumValueSelector :: Selector
minimumValueSelector = mkSelector "minimumValue"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector
setCurrentValueSelector = mkSelector "setCurrentValue:"

