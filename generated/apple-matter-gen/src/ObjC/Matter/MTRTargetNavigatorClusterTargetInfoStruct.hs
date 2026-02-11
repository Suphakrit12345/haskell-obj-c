{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetInfoStruct@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetInfoStruct
  ( MTRTargetNavigatorClusterTargetInfoStruct
  , IsMTRTargetNavigatorClusterTargetInfoStruct(..)
  , identifier
  , setIdentifier
  , name
  , setName
  , identifierSelector
  , setIdentifierSelector
  , nameSelector
  , setNameSelector


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

-- | @- identifier@
identifier :: IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct => mtrTargetNavigatorClusterTargetInfoStruct -> IO (Id NSNumber)
identifier mtrTargetNavigatorClusterTargetInfoStruct  =
    sendMsg mtrTargetNavigatorClusterTargetInfoStruct (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct, IsNSNumber value) => mtrTargetNavigatorClusterTargetInfoStruct -> value -> IO ()
setIdentifier mtrTargetNavigatorClusterTargetInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetInfoStruct (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct => mtrTargetNavigatorClusterTargetInfoStruct -> IO (Id NSString)
name mtrTargetNavigatorClusterTargetInfoStruct  =
    sendMsg mtrTargetNavigatorClusterTargetInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct, IsNSString value) => mtrTargetNavigatorClusterTargetInfoStruct -> value -> IO ()
setName mtrTargetNavigatorClusterTargetInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

