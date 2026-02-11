{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific attribute on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRAttributePath@.
module ObjC.Matter.MTRAttributePath
  ( MTRAttributePath
  , IsMTRAttributePath(..)
  , attributePathWithEndpointID_clusterID_attributeID
  , attributePathWithEndpointId_clusterId_attributeId
  , attribute
  , attributePathWithEndpointID_clusterID_attributeIDSelector
  , attributePathWithEndpointId_clusterId_attributeIdSelector
  , attributeSelector


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

-- | @+ attributePathWithEndpointID:clusterID:attributeID:@
attributePathWithEndpointID_clusterID_attributeID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID) => endpointID -> clusterID -> attributeID -> IO (Id MTRAttributePath)
attributePathWithEndpointID_clusterID_attributeID endpointID clusterID attributeID =
  do
    cls' <- getRequiredClass "MTRAttributePath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        withObjCPtr attributeID $ \raw_attributeID ->
          sendClassMsg cls' (mkSelector "attributePathWithEndpointID:clusterID:attributeID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ())] >>= retainedObject . castPtr

-- | @+ attributePathWithEndpointId:clusterId:attributeId:@
attributePathWithEndpointId_clusterId_attributeId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId) => endpointId -> clusterId -> attributeId -> IO (Id MTRAttributePath)
attributePathWithEndpointId_clusterId_attributeId endpointId clusterId attributeId =
  do
    cls' <- getRequiredClass "MTRAttributePath"
    withObjCPtr endpointId $ \raw_endpointId ->
      withObjCPtr clusterId $ \raw_clusterId ->
        withObjCPtr attributeId $ \raw_attributeId ->
          sendClassMsg cls' (mkSelector "attributePathWithEndpointId:clusterId:attributeId:") (retPtr retVoid) [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_attributeId :: Ptr ())] >>= retainedObject . castPtr

-- | @- attribute@
attribute :: IsMTRAttributePath mtrAttributePath => mtrAttributePath -> IO (Id NSNumber)
attribute mtrAttributePath  =
    sendMsg mtrAttributePath (mkSelector "attribute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributePathWithEndpointID:clusterID:attributeID:@
attributePathWithEndpointID_clusterID_attributeIDSelector :: Selector
attributePathWithEndpointID_clusterID_attributeIDSelector = mkSelector "attributePathWithEndpointID:clusterID:attributeID:"

-- | @Selector@ for @attributePathWithEndpointId:clusterId:attributeId:@
attributePathWithEndpointId_clusterId_attributeIdSelector :: Selector
attributePathWithEndpointId_clusterId_attributeIdSelector = mkSelector "attributePathWithEndpointId:clusterId:attributeId:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

