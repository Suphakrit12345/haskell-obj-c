{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific event that can be emitted on a device (i.e. without any wildcards).  There can be multiple instances of actual events for a given event path.
--
-- Generated bindings for @MTREventPath@.
module ObjC.Matter.MTREventPath
  ( MTREventPath
  , IsMTREventPath(..)
  , eventPathWithEndpointID_clusterID_eventID
  , eventPathWithEndpointId_clusterId_eventId
  , event
  , eventPathWithEndpointID_clusterID_eventIDSelector
  , eventPathWithEndpointId_clusterId_eventIdSelector
  , eventSelector


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

-- | @+ eventPathWithEndpointID:clusterID:eventID:@
eventPathWithEndpointID_clusterID_eventID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID) => endpointID -> clusterID -> eventID -> IO (Id MTREventPath)
eventPathWithEndpointID_clusterID_eventID endpointID clusterID eventID =
  do
    cls' <- getRequiredClass "MTREventPath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        withObjCPtr eventID $ \raw_eventID ->
          sendClassMsg cls' (mkSelector "eventPathWithEndpointID:clusterID:eventID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_eventID :: Ptr ())] >>= retainedObject . castPtr

-- | @+ eventPathWithEndpointId:clusterId:eventId:@
eventPathWithEndpointId_clusterId_eventId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber eventId) => endpointId -> clusterId -> eventId -> IO (Id MTREventPath)
eventPathWithEndpointId_clusterId_eventId endpointId clusterId eventId =
  do
    cls' <- getRequiredClass "MTREventPath"
    withObjCPtr endpointId $ \raw_endpointId ->
      withObjCPtr clusterId $ \raw_clusterId ->
        withObjCPtr eventId $ \raw_eventId ->
          sendClassMsg cls' (mkSelector "eventPathWithEndpointId:clusterId:eventId:") (retPtr retVoid) [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_eventId :: Ptr ())] >>= retainedObject . castPtr

-- | @- event@
event :: IsMTREventPath mtrEventPath => mtrEventPath -> IO (Id NSNumber)
event mtrEventPath  =
    sendMsg mtrEventPath (mkSelector "event") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eventPathWithEndpointID:clusterID:eventID:@
eventPathWithEndpointID_clusterID_eventIDSelector :: Selector
eventPathWithEndpointID_clusterID_eventIDSelector = mkSelector "eventPathWithEndpointID:clusterID:eventID:"

-- | @Selector@ for @eventPathWithEndpointId:clusterId:eventId:@
eventPathWithEndpointId_clusterId_eventIdSelector :: Selector
eventPathWithEndpointId_clusterId_eventIdSelector = mkSelector "eventPathWithEndpointId:clusterId:eventId:"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

