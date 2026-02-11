{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterFabricRestrictionReviewUpdateEvent@.
module ObjC.Matter.MTRAccessControlClusterFabricRestrictionReviewUpdateEvent
  ( MTRAccessControlClusterFabricRestrictionReviewUpdateEvent
  , IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent(..)
  , token
  , setToken
  , instruction
  , setInstruction
  , arlRequestFlowUrl
  , setArlRequestFlowUrl
  , fabricIndex
  , setFabricIndex
  , tokenSelector
  , setTokenSelector
  , instructionSelector
  , setInstructionSelector
  , arlRequestFlowUrlSelector
  , setArlRequestFlowUrlSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- token@
token :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSNumber)
token mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  =
    sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "token") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToken:@
setToken :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSNumber value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setToken mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "setToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- instruction@
instruction :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSString)
instruction mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  =
    sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "instruction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInstruction:@
setInstruction :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSString value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setInstruction mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "setInstruction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arlRequestFlowUrl@
arlRequestFlowUrl :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSString)
arlRequestFlowUrl mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  =
    sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "arlRequestFlowUrl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArlRequestFlowUrl:@
setArlRequestFlowUrl :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSString value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setArlRequestFlowUrl mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "setArlRequestFlowUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  =
    sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSNumber value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterFabricRestrictionReviewUpdateEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterFabricRestrictionReviewUpdateEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @token@
tokenSelector :: Selector
tokenSelector = mkSelector "token"

-- | @Selector@ for @setToken:@
setTokenSelector :: Selector
setTokenSelector = mkSelector "setToken:"

-- | @Selector@ for @instruction@
instructionSelector :: Selector
instructionSelector = mkSelector "instruction"

-- | @Selector@ for @setInstruction:@
setInstructionSelector :: Selector
setInstructionSelector = mkSelector "setInstruction:"

-- | @Selector@ for @arlRequestFlowUrl@
arlRequestFlowUrlSelector :: Selector
arlRequestFlowUrlSelector = mkSelector "arlRequestFlowUrl"

-- | @Selector@ for @setArlRequestFlowUrl:@
setArlRequestFlowUrlSelector :: Selector
setArlRequestFlowUrlSelector = mkSelector "setArlRequestFlowUrl:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

