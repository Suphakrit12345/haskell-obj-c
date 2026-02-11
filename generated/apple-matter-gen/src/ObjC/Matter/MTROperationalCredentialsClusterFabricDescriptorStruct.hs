{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterFabricDescriptorStruct@.
module ObjC.Matter.MTROperationalCredentialsClusterFabricDescriptorStruct
  ( MTROperationalCredentialsClusterFabricDescriptorStruct
  , IsMTROperationalCredentialsClusterFabricDescriptorStruct(..)
  , rootPublicKey
  , setRootPublicKey
  , vendorID
  , setVendorID
  , vendorId
  , setVendorId
  , fabricID
  , setFabricID
  , fabricId
  , setFabricId
  , nodeID
  , setNodeID
  , nodeId
  , setNodeId
  , label
  , setLabel
  , vidVerificationStatement
  , setVidVerificationStatement
  , fabricIndex
  , setFabricIndex
  , rootPublicKeySelector
  , setRootPublicKeySelector
  , vendorIDSelector
  , setVendorIDSelector
  , vendorIdSelector
  , setVendorIdSelector
  , fabricIDSelector
  , setFabricIDSelector
  , fabricIdSelector
  , setFabricIdSelector
  , nodeIDSelector
  , setNodeIDSelector
  , nodeIdSelector
  , setNodeIdSelector
  , labelSelector
  , setLabelSelector
  , vidVerificationStatementSelector
  , setVidVerificationStatementSelector
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

-- | @- rootPublicKey@
rootPublicKey :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSData)
rootPublicKey mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "rootPublicKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRootPublicKey:@
setRootPublicKey :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setRootPublicKey mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setRootPublicKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorID@
vendorID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
vendorID mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVendorID mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorId@
vendorId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
vendorId mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "vendorId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorId:@
setVendorId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVendorId mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setVendorId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricID@
fabricID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricID mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "fabricID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricID:@
setFabricID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricID mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setFabricID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricId@
fabricId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricId mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "fabricId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricId:@
setFabricId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricId mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setFabricId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
nodeID mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setNodeID mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeId@
nodeId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
nodeId mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "nodeId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeId:@
setNodeId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setNodeId mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setNodeId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSString)
label mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSString value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setLabel mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vidVerificationStatement@
vidVerificationStatement :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSData)
vidVerificationStatement mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "vidVerificationStatement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVidVerificationStatement:@
setVidVerificationStatement :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVidVerificationStatement mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setVidVerificationStatement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterFabricDescriptorStruct  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterFabricDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptorStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @setRootPublicKey:@
setRootPublicKeySelector :: Selector
setRootPublicKeySelector = mkSelector "setRootPublicKey:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector
setVendorIdSelector = mkSelector "setVendorId:"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @setFabricID:@
setFabricIDSelector :: Selector
setFabricIDSelector = mkSelector "setFabricID:"

-- | @Selector@ for @fabricId@
fabricIdSelector :: Selector
fabricIdSelector = mkSelector "fabricId"

-- | @Selector@ for @setFabricId:@
setFabricIdSelector :: Selector
setFabricIdSelector = mkSelector "setFabricId:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @nodeId@
nodeIdSelector :: Selector
nodeIdSelector = mkSelector "nodeId"

-- | @Selector@ for @setNodeId:@
setNodeIdSelector :: Selector
setNodeIdSelector = mkSelector "setNodeId:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @vidVerificationStatement@
vidVerificationStatementSelector :: Selector
vidVerificationStatementSelector = mkSelector "vidVerificationStatement"

-- | @Selector@ for @setVidVerificationStatement:@
setVidVerificationStatementSelector :: Selector
setVidVerificationStatementSelector = mkSelector "setVidVerificationStatement:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

