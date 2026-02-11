{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterProductAppearanceStruct@.
module ObjC.Matter.MTRBasicInformationClusterProductAppearanceStruct
  ( MTRBasicInformationClusterProductAppearanceStruct
  , IsMTRBasicInformationClusterProductAppearanceStruct(..)
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
finish :: IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct => mtrBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
finish mtrBasicInformationClusterProductAppearanceStruct  =
    sendMsg mtrBasicInformationClusterProductAppearanceStruct (mkSelector "finish") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFinish:@
setFinish :: (IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setFinish mtrBasicInformationClusterProductAppearanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicInformationClusterProductAppearanceStruct (mkSelector "setFinish:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- primaryColor@
primaryColor :: IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct => mtrBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
primaryColor mtrBasicInformationClusterProductAppearanceStruct  =
    sendMsg mtrBasicInformationClusterProductAppearanceStruct (mkSelector "primaryColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryColor:@
setPrimaryColor :: (IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setPrimaryColor mtrBasicInformationClusterProductAppearanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicInformationClusterProductAppearanceStruct (mkSelector "setPrimaryColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

