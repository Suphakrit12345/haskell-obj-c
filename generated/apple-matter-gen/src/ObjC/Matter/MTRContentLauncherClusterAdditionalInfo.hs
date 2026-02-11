{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterAdditionalInfo@.
module ObjC.Matter.MTRContentLauncherClusterAdditionalInfo
  ( MTRContentLauncherClusterAdditionalInfo
  , IsMTRContentLauncherClusterAdditionalInfo(..)
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
name :: IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo => mtrContentLauncherClusterAdditionalInfo -> IO (Id NSString)
name mtrContentLauncherClusterAdditionalInfo  =
    sendMsg mtrContentLauncherClusterAdditionalInfo (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo, IsNSString value) => mtrContentLauncherClusterAdditionalInfo -> value -> IO ()
setName mtrContentLauncherClusterAdditionalInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterAdditionalInfo (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo => mtrContentLauncherClusterAdditionalInfo -> IO (Id NSString)
value mtrContentLauncherClusterAdditionalInfo  =
    sendMsg mtrContentLauncherClusterAdditionalInfo (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo, IsNSString value) => mtrContentLauncherClusterAdditionalInfo -> value -> IO ()
setValue mtrContentLauncherClusterAdditionalInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterAdditionalInfo (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

