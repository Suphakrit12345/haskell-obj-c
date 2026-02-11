{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific command on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRCommandPath@.
module ObjC.Matter.MTRCommandPath
  ( MTRCommandPath
  , IsMTRCommandPath(..)
  , commandPathWithEndpointID_clusterID_commandID
  , commandPathWithEndpointId_clusterId_commandId
  , command
  , commandPathWithEndpointID_clusterID_commandIDSelector
  , commandPathWithEndpointId_clusterId_commandIdSelector
  , commandSelector


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

-- | @+ commandPathWithEndpointID:clusterID:commandID:@
commandPathWithEndpointID_clusterID_commandID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID) => endpointID -> clusterID -> commandID -> IO (Id MTRCommandPath)
commandPathWithEndpointID_clusterID_commandID endpointID clusterID commandID =
  do
    cls' <- getRequiredClass "MTRCommandPath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        withObjCPtr commandID $ \raw_commandID ->
          sendClassMsg cls' (mkSelector "commandPathWithEndpointID:clusterID:commandID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_commandID :: Ptr ())] >>= retainedObject . castPtr

-- | @+ commandPathWithEndpointId:clusterId:commandId:@
commandPathWithEndpointId_clusterId_commandId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber commandId) => endpointId -> clusterId -> commandId -> IO (Id MTRCommandPath)
commandPathWithEndpointId_clusterId_commandId endpointId clusterId commandId =
  do
    cls' <- getRequiredClass "MTRCommandPath"
    withObjCPtr endpointId $ \raw_endpointId ->
      withObjCPtr clusterId $ \raw_clusterId ->
        withObjCPtr commandId $ \raw_commandId ->
          sendClassMsg cls' (mkSelector "commandPathWithEndpointId:clusterId:commandId:") (retPtr retVoid) [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_commandId :: Ptr ())] >>= retainedObject . castPtr

-- | @- command@
command :: IsMTRCommandPath mtrCommandPath => mtrCommandPath -> IO (Id NSNumber)
command mtrCommandPath  =
    sendMsg mtrCommandPath (mkSelector "command") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandPathWithEndpointID:clusterID:commandID:@
commandPathWithEndpointID_clusterID_commandIDSelector :: Selector
commandPathWithEndpointID_clusterID_commandIDSelector = mkSelector "commandPathWithEndpointID:clusterID:commandID:"

-- | @Selector@ for @commandPathWithEndpointId:clusterId:commandId:@
commandPathWithEndpointId_clusterId_commandIdSelector :: Selector
commandPathWithEndpointId_clusterId_commandIdSelector = mkSelector "commandPathWithEndpointId:clusterId:commandId:"

-- | @Selector@ for @command@
commandSelector :: Selector
commandSelector = mkSelector "command"

