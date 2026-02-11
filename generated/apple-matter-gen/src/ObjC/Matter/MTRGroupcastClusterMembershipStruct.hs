{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterMembershipStruct@.
module ObjC.Matter.MTRGroupcastClusterMembershipStruct
  ( MTRGroupcastClusterMembershipStruct
  , IsMTRGroupcastClusterMembershipStruct(..)
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , keyID
  , setKeyID
  , hasAuxiliaryACL
  , setHasAuxiliaryACL
  , expiringKeyID
  , setExpiringKeyID
  , fabricIndex
  , setFabricIndex
  , groupIDSelector
  , setGroupIDSelector
  , endpointsSelector
  , setEndpointsSelector
  , keyIDSelector
  , setKeyIDSelector
  , hasAuxiliaryACLSelector
  , setHasAuxiliaryACLSelector
  , expiringKeyIDSelector
  , setExpiringKeyIDSelector
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

-- | @- groupID@
groupID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
groupID mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setGroupID mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSArray)
endpoints mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "endpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSArray value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setEndpoints mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setEndpoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyID@
keyID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
keyID mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "keyID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setKeyID mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setKeyID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasAuxiliaryACL@
hasAuxiliaryACL :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
hasAuxiliaryACL mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "hasAuxiliaryACL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHasAuxiliaryACL:@
setHasAuxiliaryACL :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setHasAuxiliaryACL mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setHasAuxiliaryACL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expiringKeyID@
expiringKeyID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
expiringKeyID mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "expiringKeyID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpiringKeyID:@
setExpiringKeyID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setExpiringKeyID mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setExpiringKeyID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
fabricIndex mtrGroupcastClusterMembershipStruct  =
    sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setFabricIndex mtrGroupcastClusterMembershipStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterMembershipStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @keyID@
keyIDSelector :: Selector
keyIDSelector = mkSelector "keyID"

-- | @Selector@ for @setKeyID:@
setKeyIDSelector :: Selector
setKeyIDSelector = mkSelector "setKeyID:"

-- | @Selector@ for @hasAuxiliaryACL@
hasAuxiliaryACLSelector :: Selector
hasAuxiliaryACLSelector = mkSelector "hasAuxiliaryACL"

-- | @Selector@ for @setHasAuxiliaryACL:@
setHasAuxiliaryACLSelector :: Selector
setHasAuxiliaryACLSelector = mkSelector "setHasAuxiliaryACL:"

-- | @Selector@ for @expiringKeyID@
expiringKeyIDSelector :: Selector
expiringKeyIDSelector = mkSelector "expiringKeyID"

-- | @Selector@ for @setExpiringKeyID:@
setExpiringKeyIDSelector :: Selector
setExpiringKeyIDSelector = mkSelector "setExpiringKeyID:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

