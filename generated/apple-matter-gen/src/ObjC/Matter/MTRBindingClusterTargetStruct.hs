{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBindingClusterTargetStruct@.
module ObjC.Matter.MTRBindingClusterTargetStruct
  ( MTRBindingClusterTargetStruct
  , IsMTRBindingClusterTargetStruct(..)
  , node
  , setNode
  , group
  , setGroup
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , fabricIndex
  , setFabricIndex
  , nodeSelector
  , setNodeSelector
  , groupSelector
  , setGroupSelector
  , endpointSelector
  , setEndpointSelector
  , clusterSelector
  , setClusterSelector
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

-- | @- node@
node :: IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct => mtrBindingClusterTargetStruct -> IO (Id NSNumber)
node mtrBindingClusterTargetStruct  =
    sendMsg mtrBindingClusterTargetStruct (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNode:@
setNode :: (IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct, IsNSNumber value) => mtrBindingClusterTargetStruct -> value -> IO ()
setNode mtrBindingClusterTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBindingClusterTargetStruct (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- group@
group :: IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct => mtrBindingClusterTargetStruct -> IO (Id NSNumber)
group mtrBindingClusterTargetStruct  =
    sendMsg mtrBindingClusterTargetStruct (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroup:@
setGroup :: (IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct, IsNSNumber value) => mtrBindingClusterTargetStruct -> value -> IO ()
setGroup mtrBindingClusterTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBindingClusterTargetStruct (mkSelector "setGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct => mtrBindingClusterTargetStruct -> IO (Id NSNumber)
endpoint mtrBindingClusterTargetStruct  =
    sendMsg mtrBindingClusterTargetStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct, IsNSNumber value) => mtrBindingClusterTargetStruct -> value -> IO ()
setEndpoint mtrBindingClusterTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBindingClusterTargetStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cluster@
cluster :: IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct => mtrBindingClusterTargetStruct -> IO (Id NSNumber)
cluster mtrBindingClusterTargetStruct  =
    sendMsg mtrBindingClusterTargetStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct, IsNSNumber value) => mtrBindingClusterTargetStruct -> value -> IO ()
setCluster mtrBindingClusterTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBindingClusterTargetStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct => mtrBindingClusterTargetStruct -> IO (Id NSNumber)
fabricIndex mtrBindingClusterTargetStruct  =
    sendMsg mtrBindingClusterTargetStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRBindingClusterTargetStruct mtrBindingClusterTargetStruct, IsNSNumber value) => mtrBindingClusterTargetStruct -> value -> IO ()
setFabricIndex mtrBindingClusterTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBindingClusterTargetStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

