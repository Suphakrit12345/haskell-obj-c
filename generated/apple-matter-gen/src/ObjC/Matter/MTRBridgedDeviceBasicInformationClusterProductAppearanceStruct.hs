{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct
  ( MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct
  , IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct(..)
  , finish
  , setFinish
  , primaryColor
  , setPrimaryColor
  , finishSelector
  , setFinishSelector
  , primaryColorSelector
  , setPrimaryColorSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- finish@
finish :: IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
finish mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct  =
    sendMsg mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct (mkSelector "finish") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFinish:@
setFinish :: (IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setFinish mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct (mkSelector "setFinish:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- primaryColor@
primaryColor :: IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
primaryColor mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct  =
    sendMsg mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct (mkSelector "primaryColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryColor:@
setPrimaryColor :: (IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setPrimaryColor mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct (mkSelector "setPrimaryColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finish@
finishSelector :: Selector
finishSelector = mkSelector "finish"

-- | @Selector@ for @setFinish:@
setFinishSelector :: Selector
setFinishSelector = mkSelector "setFinish:"

-- | @Selector@ for @primaryColor@
primaryColorSelector :: Selector
primaryColorSelector = mkSelector "primaryColor"

-- | @Selector@ for @setPrimaryColor:@
setPrimaryColorSelector :: Selector
setPrimaryColorSelector = mkSelector "setPrimaryColor:"

