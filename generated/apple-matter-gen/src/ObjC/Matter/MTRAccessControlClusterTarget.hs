{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterTarget@.
module ObjC.Matter.MTRAccessControlClusterTarget
  ( MTRAccessControlClusterTarget
  , IsMTRAccessControlClusterTarget(..)
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
cluster :: IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget => mtrAccessControlClusterTarget -> IO (Id NSNumber)
cluster mtrAccessControlClusterTarget  =
    sendMsg mtrAccessControlClusterTarget (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget, IsNSNumber value) => mtrAccessControlClusterTarget -> value -> IO ()
setCluster mtrAccessControlClusterTarget  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterTarget (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget => mtrAccessControlClusterTarget -> IO (Id NSNumber)
endpoint mtrAccessControlClusterTarget  =
    sendMsg mtrAccessControlClusterTarget (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget, IsNSNumber value) => mtrAccessControlClusterTarget -> value -> IO ()
setEndpoint mtrAccessControlClusterTarget  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterTarget (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deviceType@
deviceType :: IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget => mtrAccessControlClusterTarget -> IO (Id NSNumber)
deviceType mtrAccessControlClusterTarget  =
    sendMsg mtrAccessControlClusterTarget (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceType:@
setDeviceType :: (IsMTRAccessControlClusterTarget mtrAccessControlClusterTarget, IsNSNumber value) => mtrAccessControlClusterTarget -> value -> IO ()
setDeviceType mtrAccessControlClusterTarget  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterTarget (mkSelector "setDeviceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

