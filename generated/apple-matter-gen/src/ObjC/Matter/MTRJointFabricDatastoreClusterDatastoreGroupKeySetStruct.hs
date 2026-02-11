{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct
  ( MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct(..)
  , groupKeySetID
  , setGroupKeySetID
  , groupKeySecurityPolicy
  , setGroupKeySecurityPolicy
  , epochKey0
  , setEpochKey0
  , epochStartTime0
  , setEpochStartTime0
  , epochKey1
  , setEpochKey1
  , epochStartTime1
  , setEpochStartTime1
  , epochKey2
  , setEpochKey2
  , epochStartTime2
  , setEpochStartTime2
  , groupKeyMulticastPolicy
  , setGroupKeyMulticastPolicy
  , groupKeySetIDSelector
  , setGroupKeySetIDSelector
  , groupKeySecurityPolicySelector
  , setGroupKeySecurityPolicySelector
  , epochKey0Selector
  , setEpochKey0Selector
  , epochStartTime0Selector
  , setEpochStartTime0Selector
  , epochKey1Selector
  , setEpochKey1Selector
  , epochStartTime1Selector
  , setEpochStartTime1Selector
  , epochKey2Selector
  , setEpochKey2Selector
  , epochStartTime2Selector
  , setEpochStartTime2Selector
  , groupKeyMulticastPolicySelector
  , setGroupKeyMulticastPolicySelector


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

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "groupKeySetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setGroupKeySetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeySecurityPolicy@
groupKeySecurityPolicy :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeySecurityPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "groupKeySecurityPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySecurityPolicy:@
setGroupKeySecurityPolicy :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeySecurityPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setGroupKeySecurityPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochKey0@
epochKey0 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochKey0") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochKey0:@
setEpochKey0 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochKey0:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochStartTime0@
epochStartTime0 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochStartTime0") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochStartTime0:@
setEpochStartTime0 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochStartTime0:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochKey1@
epochKey1 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochKey1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochKey1:@
setEpochKey1 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochKey1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochStartTime1@
epochStartTime1 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochStartTime1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochStartTime1:@
setEpochStartTime1 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochStartTime1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochKey2@
epochKey2 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochKey2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochKey2:@
setEpochKey2 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochKey2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- epochStartTime2@
epochStartTime2 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "epochStartTime2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEpochStartTime2:@
setEpochStartTime2 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setEpochStartTime2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeyMulticastPolicy@
groupKeyMulticastPolicy :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeyMulticastPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "groupKeyMulticastPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeyMulticastPolicy:@
setGroupKeyMulticastPolicy :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeyMulticastPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct (mkSelector "setGroupKeyMulticastPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupKeySecurityPolicy@
groupKeySecurityPolicySelector :: Selector
groupKeySecurityPolicySelector = mkSelector "groupKeySecurityPolicy"

-- | @Selector@ for @setGroupKeySecurityPolicy:@
setGroupKeySecurityPolicySelector :: Selector
setGroupKeySecurityPolicySelector = mkSelector "setGroupKeySecurityPolicy:"

-- | @Selector@ for @epochKey0@
epochKey0Selector :: Selector
epochKey0Selector = mkSelector "epochKey0"

-- | @Selector@ for @setEpochKey0:@
setEpochKey0Selector :: Selector
setEpochKey0Selector = mkSelector "setEpochKey0:"

-- | @Selector@ for @epochStartTime0@
epochStartTime0Selector :: Selector
epochStartTime0Selector = mkSelector "epochStartTime0"

-- | @Selector@ for @setEpochStartTime0:@
setEpochStartTime0Selector :: Selector
setEpochStartTime0Selector = mkSelector "setEpochStartTime0:"

-- | @Selector@ for @epochKey1@
epochKey1Selector :: Selector
epochKey1Selector = mkSelector "epochKey1"

-- | @Selector@ for @setEpochKey1:@
setEpochKey1Selector :: Selector
setEpochKey1Selector = mkSelector "setEpochKey1:"

-- | @Selector@ for @epochStartTime1@
epochStartTime1Selector :: Selector
epochStartTime1Selector = mkSelector "epochStartTime1"

-- | @Selector@ for @setEpochStartTime1:@
setEpochStartTime1Selector :: Selector
setEpochStartTime1Selector = mkSelector "setEpochStartTime1:"

-- | @Selector@ for @epochKey2@
epochKey2Selector :: Selector
epochKey2Selector = mkSelector "epochKey2"

-- | @Selector@ for @setEpochKey2:@
setEpochKey2Selector :: Selector
setEpochKey2Selector = mkSelector "setEpochKey2:"

-- | @Selector@ for @epochStartTime2@
epochStartTime2Selector :: Selector
epochStartTime2Selector = mkSelector "epochStartTime2"

-- | @Selector@ for @setEpochStartTime2:@
setEpochStartTime2Selector :: Selector
setEpochStartTime2Selector = mkSelector "setEpochStartTime2:"

-- | @Selector@ for @groupKeyMulticastPolicy@
groupKeyMulticastPolicySelector :: Selector
groupKeyMulticastPolicySelector = mkSelector "groupKeyMulticastPolicy"

-- | @Selector@ for @setGroupKeyMulticastPolicy:@
setGroupKeyMulticastPolicySelector :: Selector
setGroupKeyMulticastPolicySelector = mkSelector "setGroupKeyMulticastPolicy:"

