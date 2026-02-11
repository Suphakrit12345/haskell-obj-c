{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTrustedTimeSourceStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterTrustedTimeSourceStruct
  ( MTRTimeSynchronizationClusterTrustedTimeSourceStruct
  , IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct(..)
  , fabricIndex
  , setFabricIndex
  , nodeID
  , setNodeID
  , endpoint
  , setEndpoint
  , fabricIndexSelector
  , setFabricIndexSelector
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

-- | @- fabricIndex@
fabricIndex :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
fabricIndex mtrTimeSynchronizationClusterTrustedTimeSourceStruct  =
    sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setFabricIndex mtrTimeSynchronizationClusterTrustedTimeSourceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
nodeID mtrTimeSynchronizationClusterTrustedTimeSourceStruct  =
    sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setNodeID mtrTimeSynchronizationClusterTrustedTimeSourceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
endpoint mtrTimeSynchronizationClusterTrustedTimeSourceStruct  =
    sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setEndpoint mtrTimeSynchronizationClusterTrustedTimeSourceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTrustedTimeSourceStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

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

