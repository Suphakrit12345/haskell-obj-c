{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramCastStruct@.
module ObjC.Matter.MTRChannelClusterProgramCastStruct
  ( MTRChannelClusterProgramCastStruct
  , IsMTRChannelClusterProgramCastStruct(..)
  , name
  , setName
  , role_
  , setRole
  , nameSelector
  , setNameSelector
  , roleSelector
  , setRoleSelector


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
name :: IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct => mtrChannelClusterProgramCastStruct -> IO (Id NSString)
name mtrChannelClusterProgramCastStruct  =
    sendMsg mtrChannelClusterProgramCastStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct, IsNSString value) => mtrChannelClusterProgramCastStruct -> value -> IO ()
setName mtrChannelClusterProgramCastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramCastStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- role@
role_ :: IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct => mtrChannelClusterProgramCastStruct -> IO (Id NSString)
role_ mtrChannelClusterProgramCastStruct  =
    sendMsg mtrChannelClusterProgramCastStruct (mkSelector "role") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRole:@
setRole :: (IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct, IsNSString value) => mtrChannelClusterProgramCastStruct -> value -> IO ()
setRole mtrChannelClusterProgramCastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramCastStruct (mkSelector "setRole:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @role@
roleSelector :: Selector
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector
setRoleSelector = mkSelector "setRole:"

