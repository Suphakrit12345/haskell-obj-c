{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingListParameter@.
module ObjC.MediaExtension.MERAWProcessingListParameter
  ( MERAWProcessingListParameter
  , IsMERAWProcessingListParameter(..)
  , initWithName_key_description_list_initialValue
  , initWithName_key_description_list_initialValue_neutralValue
  , initWithName_key_description_list_initialValue_cameraValue
  , initWithName_key_description_list_initialValue_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , listElements
  , initialValue
  , currentValue
  , setCurrentValue
  , initWithName_key_description_list_initialValueSelector
  , initWithName_key_description_list_initialValue_neutralValueSelector
  , initWithName_key_description_list_initialValue_cameraValueSelector
  , initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector
  , hasNeutralValueSelector
  , hasCameraValueSelector
  , listElementsSelector
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

-- | @- initWithName:key:description:list:initialValue:@
initWithName_key_description_list_initialValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue merawProcessingListParameter  name key description listElements initialValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
        withObjCPtr listElements $ \raw_listElements ->
            sendMsg merawProcessingListParameter (mkSelector "initWithName:key:description:list:initialValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_listElements :: Ptr ()), argCLong initialValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:list:initialValue:neutralValue:@
initWithName_key_description_list_initialValue_neutralValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValue merawProcessingListParameter  name key description listElements initialValue neutralValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
        withObjCPtr listElements $ \raw_listElements ->
            sendMsg merawProcessingListParameter (mkSelector "initWithName:key:description:list:initialValue:neutralValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_listElements :: Ptr ()), argCLong initialValue, argCLong neutralValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:list:initialValue:cameraValue:@
initWithName_key_description_list_initialValue_cameraValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_cameraValue merawProcessingListParameter  name key description listElements initialValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
        withObjCPtr listElements $ \raw_listElements ->
            sendMsg merawProcessingListParameter (mkSelector "initWithName:key:description:list:initialValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_listElements :: Ptr ()), argCLong initialValue, argCLong cameraValue] >>= ownedObject . castPtr

-- | @- initWithName:key:description:list:initialValue:neutralValue:cameraValue:@
initWithName_key_description_list_initialValue_neutralValue_cameraValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValue_cameraValue merawProcessingListParameter  name key description listElements initialValue neutralValue cameraValue =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr description $ \raw_description ->
        withObjCPtr listElements $ \raw_listElements ->
            sendMsg merawProcessingListParameter (mkSelector "initWithName:key:description:list:initialValue:neutralValue:cameraValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_listElements :: Ptr ()), argCLong initialValue, argCLong neutralValue, argCLong cameraValue] >>= ownedObject . castPtr

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingListParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> Ptr CLong -> IO Bool
hasNeutralValue merawProcessingListParameter  outNeutralValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingListParameter (mkSelector "hasNeutralValue:") retCULong [argPtr outNeutralValue]

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingListParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> Ptr CLong -> IO Bool
hasCameraValue merawProcessingListParameter  outCameraValue =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg merawProcessingListParameter (mkSelector "hasCameraValue:") retCULong [argPtr outCameraValue]

-- | listElements
--
-- The ordered array of MERAWProcessingListElementParameter which make up this list.
--
-- ObjC selector: @- listElements@
listElements :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO (Id NSArray)
listElements merawProcessingListParameter  =
    sendMsg merawProcessingListParameter (mkSelector "listElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.  The value is the listElementID value of the MERAWProcessingListElementParameter for initial settings.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO CLong
initialValue merawProcessingListParameter  =
    sendMsg merawProcessingListParameter (mkSelector "initialValue") retCLong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- The value is the listElementID value of the selected MERAWProcessingListElementParameter.   This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO CLong
currentValue merawProcessingListParameter  =
    sendMsg merawProcessingListParameter (mkSelector "currentValue") retCLong []

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- The value is the listElementID value of the selected MERAWProcessingListElementParameter.   This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> CLong -> IO ()
setCurrentValue merawProcessingListParameter  value =
    sendMsg merawProcessingListParameter (mkSelector "setCurrentValue:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:list:initialValue:@
initWithName_key_description_list_initialValueSelector :: Selector
initWithName_key_description_list_initialValueSelector = mkSelector "initWithName:key:description:list:initialValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:neutralValue:@
initWithName_key_description_list_initialValue_neutralValueSelector :: Selector
initWithName_key_description_list_initialValue_neutralValueSelector = mkSelector "initWithName:key:description:list:initialValue:neutralValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:cameraValue:@
initWithName_key_description_list_initialValue_cameraValueSelector :: Selector
initWithName_key_description_list_initialValue_cameraValueSelector = mkSelector "initWithName:key:description:list:initialValue:cameraValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:neutralValue:cameraValue:@
initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector :: Selector
initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:list:initialValue:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @listElements@
listElementsSelector :: Selector
listElementsSelector = mkSelector "listElements"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector
setCurrentValueSelector = mkSelector "setCurrentValue:"

