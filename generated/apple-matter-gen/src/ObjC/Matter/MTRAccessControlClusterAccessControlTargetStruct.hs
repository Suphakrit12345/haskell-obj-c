{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlTargetStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlTargetStruct
  ( MTRAccessControlClusterAccessControlTargetStruct
  , IsMTRAccessControlClusterAccessControlTargetStruct(..)
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
cluster :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterAccessControlTargetStruct  =
    sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setCluster mtrAccessControlClusterAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterAccessControlTargetStruct  =
    sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deviceType@
deviceType :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
deviceType mtrAccessControlClusterAccessControlTargetStruct  =
    sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceType:@
setDeviceType :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setDeviceType mtrAccessControlClusterAccessControlTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlTargetStruct (mkSelector "setDeviceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

