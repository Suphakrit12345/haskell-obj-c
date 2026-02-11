{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of an endpoint implemented by an MTRDeviceController.
--
-- MTRServerEndpoint's API can be accessed from any thread.
--
-- Generated bindings for @MTRServerEndpoint@.
module ObjC.Matter.MTRServerEndpoint
  ( MTRServerEndpoint
  , IsMTRServerEndpoint(..)
  , init_
  , new
  , initWithEndpointID_deviceTypes
  , addAccessGrant
  , removeAccessGrant
  , addServerCluster
  , endpointID
  , deviceTypes
  , accessGrants
  , serverClusters
  , initSelector
  , newSelector
  , initWithEndpointID_deviceTypesSelector
  , addAccessGrantSelector
  , removeAccessGrantSelector
  , addServerClusterSelector
  , endpointIDSelector
  , deviceTypesSelector
  , accessGrantsSelector
  , serverClustersSelector


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
init_ :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id MTRServerEndpoint)
init_ mtrServerEndpoint  =
    sendMsg mtrServerEndpoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRServerEndpoint)
new  =
  do
    cls' <- getRequiredClass "MTRServerEndpoint"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The provided endpointID must be in the range 1-65535.  The list of device types provided must be nonempty (but may include vendor-specific device types).
--
-- ObjC selector: @- initWithEndpointID:deviceTypes:@
initWithEndpointID_deviceTypes :: (IsMTRServerEndpoint mtrServerEndpoint, IsNSNumber endpointID, IsNSArray deviceTypes) => mtrServerEndpoint -> endpointID -> deviceTypes -> IO (Id MTRServerEndpoint)
initWithEndpointID_deviceTypes mtrServerEndpoint  endpointID deviceTypes =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr deviceTypes $ \raw_deviceTypes ->
        sendMsg mtrServerEndpoint (mkSelector "initWithEndpointID:deviceTypes:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_deviceTypes :: Ptr ())] >>= ownedObject . castPtr

-- | Add an access grant to the endpoint.  If the same access grant is added multiple times, it will be treated as if it were added once (and removing it once will remove it).
--
-- ObjC selector: @- addAccessGrant:@
addAccessGrant :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRAccessGrant accessGrant) => mtrServerEndpoint -> accessGrant -> IO ()
addAccessGrant mtrServerEndpoint  accessGrant =
  withObjCPtr accessGrant $ \raw_accessGrant ->
      sendMsg mtrServerEndpoint (mkSelector "addAccessGrant:") retVoid [argPtr (castPtr raw_accessGrant :: Ptr ())]

-- | Remove an access grant from the endpoint.
--
-- ObjC selector: @- removeAccessGrant:@
removeAccessGrant :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRAccessGrant accessGrant) => mtrServerEndpoint -> accessGrant -> IO ()
removeAccessGrant mtrServerEndpoint  accessGrant =
  withObjCPtr accessGrant $ \raw_accessGrant ->
      sendMsg mtrServerEndpoint (mkSelector "removeAccessGrant:") retVoid [argPtr (castPtr raw_accessGrant :: Ptr ())]

-- | Add a server cluster to the endpoint.  This can only be done before the endpoint has been added to a controller.
--
-- The cluster must not have the same cluster ID as another cluster on this endpoint.
--
-- The cluster must not already be added to another endpoint.
--
-- ObjC selector: @- addServerCluster:@
addServerCluster :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRServerCluster serverCluster) => mtrServerEndpoint -> serverCluster -> IO Bool
addServerCluster mtrServerEndpoint  serverCluster =
  withObjCPtr serverCluster $ \raw_serverCluster ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrServerEndpoint (mkSelector "addServerCluster:") retCULong [argPtr (castPtr raw_serverCluster :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSNumber)
endpointID mtrServerEndpoint  =
    sendMsg mtrServerEndpoint (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceTypes@
deviceTypes :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
deviceTypes mtrServerEndpoint  =
    sendMsg mtrServerEndpoint (mkSelector "deviceTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The list of entities that are allowed to access all clusters on this endpoint.  If more fine-grained access control is desired, access grants should be defined on individual clusters.
--
-- Defaults to empty list, which means no access granted.
--
-- ObjC selector: @- accessGrants@
accessGrants :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
accessGrants mtrServerEndpoint  =
    sendMsg mtrServerEndpoint (mkSelector "accessGrants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of server clusters supported on this endpoint.  The Descriptor cluster does not need to be included unless a TagList attribute is desired on it or it has a non-empty PartsList, or it needs to have cluster-specific access grants.  If not included, the Descriptor cluster will be generated automatically.
--
-- ObjC selector: @- serverClusters@
serverClusters :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
serverClusters mtrServerEndpoint  =
    sendMsg mtrServerEndpoint (mkSelector "serverClusters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEndpointID:deviceTypes:@
initWithEndpointID_deviceTypesSelector :: Selector
initWithEndpointID_deviceTypesSelector = mkSelector "initWithEndpointID:deviceTypes:"

-- | @Selector@ for @addAccessGrant:@
addAccessGrantSelector :: Selector
addAccessGrantSelector = mkSelector "addAccessGrant:"

-- | @Selector@ for @removeAccessGrant:@
removeAccessGrantSelector :: Selector
removeAccessGrantSelector = mkSelector "removeAccessGrant:"

-- | @Selector@ for @addServerCluster:@
addServerClusterSelector :: Selector
addServerClusterSelector = mkSelector "addServerCluster:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @accessGrants@
accessGrantsSelector :: Selector
accessGrantsSelector = mkSelector "accessGrants"

-- | @Selector@ for @serverClusters@
serverClustersSelector :: Selector
serverClustersSelector = mkSelector "serverClusters"

