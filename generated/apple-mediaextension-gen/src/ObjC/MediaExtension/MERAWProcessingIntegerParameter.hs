{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingIntegerParameter@.
module ObjC.MediaExtension.MERAWProcessingIntegerParameter
  ( MERAWProcessingIntegerParameter
  , IsMERAWProcessingIntegerParameter(..)
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
initWithName_key_description_initialValue_maximum_minimum :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum merawProcessingIntegerParameter  name key description initialValue maximum_ minimum_ =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingIntegerParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCLong initialValue, argCLong maximum_, argCLong minimum_] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue merawProcessingIntegerParameter  name key description initialValue maximum_ minimum_ neutralValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingIntegerParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCLong initialValue, argCLong maximum_, argCLong minimum_, argCLong neutralValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValue merawProcessingIntegerParameter  name key description initialValue maximum_ minimum_ cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingIntegerParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCLong initialValue, argCLong maximum_, argCLong minimum_, argCLong cameraValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue merawProcessingIntegerParameter  name key description initialValue maximum_ minimum_ neutralValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingIntegerParameter (mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCLong initialValue, argCLong maximum_, argCLong minimum_, argCLong neutralValue, argCLong cameraValue] >>= ownedObject . castPtr

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingIntegerParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> Ptr CLong -> IO Bool
hasNeutralValue merawProcessingIntegerParameter  outNeutralValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingIntegerParameter (mkSelector "hasNeutralValue:") retCULong [argPtr outNeutralValue]

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingIntegerParameter has an optional declared Camera value.				If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> Ptr CLong -> IO Bool
hasCameraValue merawProcessingIntegerParameter  outCameraValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingIntegerParameter (mkSelector "hasCameraValue:") retCULong [argPtr outCameraValue]

-- | maximumValue
--
-- The maximum value for this parameter.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
maximumValue merawProcessingIntegerParameter  =
    sendMsg merawProcessingIntegerParameter (mkSelector "maximumValue") retCLong []

-- | minimumValue
--
-- The minimum value for this parameter.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
minimumValue merawProcessingIntegerParameter  =
    sendMsg merawProcessingIntegerParameter (mkSelector "minimumValue") retCLong []

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
initialValue merawProcessingIntegerParameter  =
    sendMsg merawProcessingIntegerParameter (mkSelector "initialValue") retCLong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
currentValue merawProcessingIntegerParameter  =
    sendMsg merawProcessingIntegerParameter (mkSelector "currentValue") retCLong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> CLong -> IO ()
setCurrentValue merawProcessingIntegerParameter  value =
    sendMsg merawProcessingIntegerParameter (mkSelector "setCurrentValue:") retVoid [argCLong value]

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

