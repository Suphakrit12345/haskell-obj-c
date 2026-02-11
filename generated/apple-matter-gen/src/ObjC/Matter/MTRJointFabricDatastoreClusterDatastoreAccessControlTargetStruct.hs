{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct
  ( MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct(..)
  , cluster
  , setCluster
  , endpoint
  , setEndpoint
  , deviceType
  , setDeviceType
  , clusterSelector
  , setClusterSelector
  , endpointSelector
  , setEndpointSelector
  , deviceTypeSelector
  , setDeviceTypeSelector


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

-- | @- cluster@
cluster :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
cluster mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setCluster mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
endpoint mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setEndpoint mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deviceType@
deviceType :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
deviceType mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceType:@
setDeviceType :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setDeviceType mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct (mkSelector "setDeviceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector
setClusterSelector = mkSelector "setCluster:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector
setDeviceTypeSelector = mkSelector "setDeviceType:"

