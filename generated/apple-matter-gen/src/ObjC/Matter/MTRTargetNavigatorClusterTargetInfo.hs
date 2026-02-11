{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetInfo@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetInfo
  ( MTRTargetNavigatorClusterTargetInfo
  , IsMTRTargetNavigatorClusterTargetInfo(..)
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
identifier :: IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo => mtrTargetNavigatorClusterTargetInfo -> IO (Id NSNumber)
identifier mtrTargetNavigatorClusterTargetInfo  =
    sendMsg mtrTargetNavigatorClusterTargetInfo (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo, IsNSNumber value) => mtrTargetNavigatorClusterTargetInfo -> value -> IO ()
setIdentifier mtrTargetNavigatorClusterTargetInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetInfo (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo => mtrTargetNavigatorClusterTargetInfo -> IO (Id NSString)
name mtrTargetNavigatorClusterTargetInfo  =
    sendMsg mtrTargetNavigatorClusterTargetInfo (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo, IsNSString value) => mtrTargetNavigatorClusterTargetInfo -> value -> IO ()
setName mtrTargetNavigatorClusterTargetInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetInfo (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

