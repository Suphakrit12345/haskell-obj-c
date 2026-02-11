{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingBooleanParameter@.
module ObjC.MediaExtension.MERAWProcessingBooleanParameter
  ( MERAWProcessingBooleanParameter
  , IsMERAWProcessingBooleanParameter(..)
  , initWithName_key_description_initialValue
  , initWithName_key_description_initialValue_neutralValue
  , initWithName_key_description_initialValue_cameraValue
  , initWithName_key_description_initialValue_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , initialValue
  , currentValue
  , setCurrentValue
  , initWithName_key_description_initialValueSelector
  , initWithName_key_description_initialValue_neutralValueSelector
  , initWithName_key_description_initialValue_cameraValueSelector
  , initWithName_key_description_initialValue_neutralValue_cameraValueSelector
  , hasNeutralValueSelector
  , hasCameraValueSelector
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

-- | @- initWithName:key:description:initialValue:@
initWithName_key_description_initialValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue merawProcessingBooleanParameter  name key description initialValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingBooleanParameter (mkSelector "initWithName:key:description:initialValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCULong (if initialValue then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:neutralValue:@
initWithName_key_description_initialValue_neutralValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValue merawProcessingBooleanParameter  name key description initialValue neutralValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingBooleanParameter (mkSelector "initWithName:key:description:initialValue:neutralValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCULong (if initialValue then 1 else 0), argCULong (if neutralValue then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:cameraValue:@
initWithName_key_description_initialValue_cameraValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_cameraValue merawProcessingBooleanParameter  name key description initialValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingBooleanParameter (mkSelector "initWithName:key:description:initialValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCULong (if initialValue then 1 else 0), argCULong (if cameraValue then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithName:key:description:initialValue:neutralValue:cameraValue:@
initWithName_key_description_initialValue_neutralValue_cameraValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValue_cameraValue merawProcessingBooleanParameter  name key description initialValue neutralValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
          sendMsg merawProcessingBooleanParameter (mkSelector "initWithName:key:description:initialValue:neutralValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argCULong (if initialValue then 1 else 0), argCULong (if neutralValue then 1 else 0), argCULong (if cameraValue then 1 else 0)] >>= ownedObject . castPtr

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingBooleanParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to NO.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Ptr Bool -> IO Bool
hasNeutralValue merawProcessingBooleanParameter  outNeutralValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingBooleanParameter (mkSelector "hasNeutralValue:") retCULong [argPtr outNeutralValue]

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingBooleanParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to NO.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Ptr Bool -> IO Bool
hasCameraValue merawProcessingBooleanParameter  outCameraValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingBooleanParameter (mkSelector "hasCameraValue:") retCULong [argPtr outCameraValue]

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> IO Bool
initialValue merawProcessingBooleanParameter  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingBooleanParameter (mkSelector "initialValue") retCULong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> IO Bool
currentValue merawProcessingBooleanParameter  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingBooleanParameter (mkSelector "currentValue") retCULong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Bool -> IO ()
setCurrentValue merawProcessingBooleanParameter  value =
    sendMsg merawProcessingBooleanParameter (mkSelector "setCurrentValue:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:initialValue:@
initWithName_key_description_initialValueSelector :: Selector
initWithName_key_description_initialValueSelector = mkSelector "initWithName:key:description:initialValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:neutralValue:@
initWithName_key_description_initialValue_neutralValueSelector :: Selector
initWithName_key_description_initialValue_neutralValueSelector = mkSelector "initWithName:key:description:initialValue:neutralValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:cameraValue:@
initWithName_key_description_initialValue_cameraValueSelector :: Selector
initWithName_key_description_initialValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:cameraValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:neutralValue:cameraValue:@
initWithName_key_description_initialValue_neutralValue_cameraValueSelector :: Selector
initWithName_key_description_initialValue_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector
setCurrentValueSelector = mkSelector "setCurrentValue:"

