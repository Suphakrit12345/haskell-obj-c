{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct
  ( MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct(..)
  , node
  , setNode
  , group
  , setGroup
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , nodeSelector
  , setNodeSelector
  , groupSelector
  , setGroupSelector
  , endpointSelector
  , setEndpointSelector
  , clusterSelector
  , setClusterSelector


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

-- | @- node@
node :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
node mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNode:@
setNode :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setNode mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- group@
group :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
group mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroup:@
setGroup :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setGroup mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "setGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
endpoint mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setEndpoint mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cluster@
cluster :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
cluster mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setCluster mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector
setClusterSelector = mkSelector "setCluster:"

