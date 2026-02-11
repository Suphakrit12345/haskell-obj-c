{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterAdditionalInfoStruct@.
module ObjC.Matter.MTRContentLauncherClusterAdditionalInfoStruct
  ( MTRContentLauncherClusterAdditionalInfoStruct
  , IsMTRContentLauncherClusterAdditionalInfoStruct(..)
  , name
  , setName
  , value
  , setValue
  , nameSelector
  , setNameSelector
  , valueSelector
  , setValueSelector


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

-- | @- name@
name :: IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct => mtrContentLauncherClusterAdditionalInfoStruct -> IO (Id NSString)
name mtrContentLauncherClusterAdditionalInfoStruct  =
    sendMsg mtrContentLauncherClusterAdditionalInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct, IsNSString value) => mtrContentLauncherClusterAdditionalInfoStruct -> value -> IO ()
setName mtrContentLauncherClusterAdditionalInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterAdditionalInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct => mtrContentLauncherClusterAdditionalInfoStruct -> IO (Id NSString)
value mtrContentLauncherClusterAdditionalInfoStruct  =
    sendMsg mtrContentLauncherClusterAdditionalInfoStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct, IsNSString value) => mtrContentLauncherClusterAdditionalInfoStruct -> value -> IO ()
setValue mtrContentLauncherClusterAdditionalInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterAdditionalInfoStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

