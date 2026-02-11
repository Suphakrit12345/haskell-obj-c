{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerTopologyClusterCircuitNodeStruct@.
module ObjC.Matter.MTRPowerTopologyClusterCircuitNodeStruct
  ( MTRPowerTopologyClusterCircuitNodeStruct
  , IsMTRPowerTopologyClusterCircuitNodeStruct(..)
  , node
  , setNode
  , endpoint
  , setEndpoint
  , label
  , setLabel
  , fabricIndex
  , setFabricIndex
  , nodeSelector
  , setNodeSelector
  , endpointSelector
  , setEndpointSelector
  , labelSelector
  , setLabelSelector
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
node :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
node mtrPowerTopologyClusterCircuitNodeStruct  =
    sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNode:@
setNode :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setNode mtrPowerTopologyClusterCircuitNodeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
endpoint mtrPowerTopologyClusterCircuitNodeStruct  =
    sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setEndpoint mtrPowerTopologyClusterCircuitNodeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSString)
label mtrPowerTopologyClusterCircuitNodeStruct  =
    sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSString value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setLabel mtrPowerTopologyClusterCircuitNodeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
fabricIndex mtrPowerTopologyClusterCircuitNodeStruct  =
    sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setFabricIndex mtrPowerTopologyClusterCircuitNodeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerTopologyClusterCircuitNodeStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

