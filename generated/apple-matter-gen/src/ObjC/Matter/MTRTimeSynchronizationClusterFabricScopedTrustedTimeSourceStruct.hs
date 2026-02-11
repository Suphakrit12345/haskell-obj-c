{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct
  ( MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct
  , IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct(..)
  , nodeID
  , setNodeID
  , endpoint
  , setEndpoint
  , nodeIDSelector
  , setNodeIDSelector
  , endpointSelector
  , setEndpointSelector


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

-- | @- nodeID@
nodeID :: IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> IO (Id NSNumber)
nodeID mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct  =
    sendMsg mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> value -> IO ()
setNodeID mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> IO (Id NSNumber)
endpoint mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct  =
    sendMsg mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> value -> IO ()
setEndpoint mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

