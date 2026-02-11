{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterAdditionalInfoStruct@.
module ObjC.Matter.MTRChannelClusterAdditionalInfoStruct
  ( MTRChannelClusterAdditionalInfoStruct
  , IsMTRChannelClusterAdditionalInfoStruct(..)
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
name :: IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct => mtrChannelClusterAdditionalInfoStruct -> IO (Id NSString)
name mtrChannelClusterAdditionalInfoStruct  =
    sendMsg mtrChannelClusterAdditionalInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct, IsNSString value) => mtrChannelClusterAdditionalInfoStruct -> value -> IO ()
setName mtrChannelClusterAdditionalInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterAdditionalInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct => mtrChannelClusterAdditionalInfoStruct -> IO (Id NSString)
value mtrChannelClusterAdditionalInfoStruct  =
    sendMsg mtrChannelClusterAdditionalInfoStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct, IsNSString value) => mtrChannelClusterAdditionalInfoStruct -> value -> IO ()
setValue mtrChannelClusterAdditionalInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterAdditionalInfoStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

