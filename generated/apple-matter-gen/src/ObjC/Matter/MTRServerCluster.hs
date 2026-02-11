{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a server cluster implemented by an MTRDeviceController.
--
-- MTRServerCluster's API can be accessed from any thread.
--
-- Generated bindings for @MTRServerCluster@.
module ObjC.Matter.MTRServerCluster
  ( MTRServerCluster
  , IsMTRServerCluster(..)
  , init_
  , new
  , initWithClusterID_revision
  , addAccessGrant
  , removeAccessGrant
  , addAttribute
  , newDescriptorCluster
  , clusterID
  , clusterRevision
  , accessGrants
  , attributes
  , initSelector
  , newSelector
  , initWithClusterID_revisionSelector
  , addAccessGrantSelector
  , removeAccessGrantSelector
  , addAttributeSelector
  , newDescriptorClusterSelector
  , clusterIDSelector
  , clusterRevisionSelector
  , accessGrantsSelector
  , attributesSelector


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

-- | @- init@
init_ :: IsMTRServerCluster mtrServerCluster => mtrServerCluster -> IO (Id MTRServerCluster)
init_ mtrServerCluster  =
    sendMsg mtrServerCluster (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRServerCluster)
new  =
  do
    cls' <- getRequiredClass "MTRServerCluster"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The provided clusterID must not be MTRClusterIDTypeDescriptorID; see newDescriptorCluster.
--
-- Otherwise, it must be a valid cluster identifier.  That means:
--
-- * In the range 0-0x7FFF for standard clusters. * In the range 0xVVVVFC00-0xVVVVFFFE for vendor-specific clusters, where VVVV   is the vendor identifier.
--
-- The provided revision must be in the range 1-65535.
--
-- ObjC selector: @- initWithClusterID:revision:@
initWithClusterID_revision :: (IsMTRServerCluster mtrServerCluster, IsNSNumber clusterID, IsNSNumber revision) => mtrServerCluster -> clusterID -> revision -> IO (Id MTRServerCluster)
initWithClusterID_revision mtrServerCluster  clusterID revision =
  withObjCPtr clusterID $ \raw_clusterID ->
    withObjCPtr revision $ \raw_revision ->
        sendMsg mtrServerCluster (mkSelector "initWithClusterID:revision:") (retPtr retVoid) [argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_revision :: Ptr ())] >>= ownedObject . castPtr

-- | Add an access grant to the cluster.  If the same access grant is added multiple times, it will be treated as if it were added once (and removing it once will remove it).
--
-- ObjC selector: @- addAccessGrant:@
addAccessGrant :: (IsMTRServerCluster mtrServerCluster, IsMTRAccessGrant accessGrant) => mtrServerCluster -> accessGrant -> IO ()
addAccessGrant mtrServerCluster  accessGrant =
  withObjCPtr accessGrant $ \raw_accessGrant ->
      sendMsg mtrServerCluster (mkSelector "addAccessGrant:") retVoid [argPtr (castPtr raw_accessGrant :: Ptr ())]

-- | Remove an access grant from the cluster.
--
-- ObjC selector: @- removeAccessGrant:@
removeAccessGrant :: (IsMTRServerCluster mtrServerCluster, IsMTRAccessGrant accessGrant) => mtrServerCluster -> accessGrant -> IO ()
removeAccessGrant mtrServerCluster  accessGrant =
  withObjCPtr accessGrant $ \raw_accessGrant ->
      sendMsg mtrServerCluster (mkSelector "removeAccessGrant:") retVoid [argPtr (castPtr raw_accessGrant :: Ptr ())]

-- | Add an attribute to the cluster.  This can only be done before the endpoint the cluster is a part of has been added to a controller.
--
-- The attribute must not have the same attribute ID as another attribute in this cluster.
--
-- The attribute must not already be added to another cluster.
--
-- If this cluster is the Descriptor cluster (id MTRClusterIDTypeDescriptorID), it must not define any values for DeviceTypeList, ServerList, ClientList, PartsList; those values will be determined automatically.
--
-- For all clusters, the global AttributeList, AcceptedCommandList, GeneratedCommandList attributes will be determined automatically and must not be included in the attributes added on the cluster.
--
-- For all clusters, the FeatureMap attribute will be assumed to be 0 unless otherwise specified and may be omitted from the attributes added to the cluster.
--
-- For all clusters, ClusterRevision will be determined automatically based on this object's clusterRevision property, and must not be explicitly added to the cluster.
--
-- ObjC selector: @- addAttribute:@
addAttribute :: (IsMTRServerCluster mtrServerCluster, IsMTRServerAttribute attribute) => mtrServerCluster -> attribute -> IO Bool
addAttribute mtrServerCluster  attribute =
  withObjCPtr attribute $ \raw_attribute ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrServerCluster (mkSelector "addAttribute:") retCULong [argPtr (castPtr raw_attribute :: Ptr ())]

-- | Create a cluster description for the descriptor cluster.  This will set clusterRevision to the current version implemented by Matter.framework.
--
-- ObjC selector: @+ newDescriptorCluster@
newDescriptorCluster :: IO (Id MTRServerCluster)
newDescriptorCluster  =
  do
    cls' <- getRequiredClass "MTRServerCluster"
    sendClassMsg cls' (mkSelector "newDescriptorCluster") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- clusterID@
clusterID :: IsMTRServerCluster mtrServerCluster => mtrServerCluster -> IO (Id NSNumber)
clusterID mtrServerCluster  =
    sendMsg mtrServerCluster (mkSelector "clusterID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clusterRevision@
clusterRevision :: IsMTRServerCluster mtrServerCluster => mtrServerCluster -> IO (Id NSNumber)
clusterRevision mtrServerCluster  =
    sendMsg mtrServerCluster (mkSelector "clusterRevision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The list of entities that are allowed to access this cluster instance.  This list is in addition to any endpoint-wide access grants that exist.
--
-- Defaults to empty list, which means no additional access grants.
--
-- ObjC selector: @- accessGrants@
accessGrants :: IsMTRServerCluster mtrServerCluster => mtrServerCluster -> IO (Id NSArray)
accessGrants mtrServerCluster  =
    sendMsg mtrServerCluster (mkSelector "accessGrants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The list of attributes supported by the cluster.
--
-- ObjC selector: @- attributes@
attributes :: IsMTRServerCluster mtrServerCluster => mtrServerCluster -> IO (Id NSArray)
attributes mtrServerCluster  =
    sendMsg mtrServerCluster (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithClusterID:revision:@
initWithClusterID_revisionSelector :: Selector
initWithClusterID_revisionSelector = mkSelector "initWithClusterID:revision:"

-- | @Selector@ for @addAccessGrant:@
addAccessGrantSelector :: Selector
addAccessGrantSelector = mkSelector "addAccessGrant:"

-- | @Selector@ for @removeAccessGrant:@
removeAccessGrantSelector :: Selector
removeAccessGrantSelector = mkSelector "removeAccessGrant:"

-- | @Selector@ for @addAttribute:@
addAttributeSelector :: Selector
addAttributeSelector = mkSelector "addAttribute:"

-- | @Selector@ for @newDescriptorCluster@
newDescriptorClusterSelector :: Selector
newDescriptorClusterSelector = mkSelector "newDescriptorCluster"

-- | @Selector@ for @clusterID@
clusterIDSelector :: Selector
clusterIDSelector = mkSelector "clusterID"

-- | @Selector@ for @clusterRevision@
clusterRevisionSelector :: Selector
clusterRevisionSelector = mkSelector "clusterRevision"

-- | @Selector@ for @accessGrants@
accessGrantsSelector :: Selector
accessGrantsSelector = mkSelector "accessGrants"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

