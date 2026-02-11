{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating an event being requested (for read or subscribe).
--
-- nil is used to represent wildcards.
--
-- Generated bindings for @MTREventRequestPath@.
module ObjC.Matter.MTREventRequestPath
  ( MTREventRequestPath
  , IsMTREventRequestPath(..)
  , requestPathWithEndpointID_clusterID_eventID
  , endpoint
  , cluster
  , event
  , requestPathWithEndpointID_clusterID_eventIDSelector
  , endpointSelector
  , clusterSelector
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

-- | @+ requestPathWithEndpointID:clusterID:eventID:@
requestPathWithEndpointID_clusterID_eventID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID) => endpointID -> clusterID -> eventID -> IO (Id MTREventRequestPath)
requestPathWithEndpointID_clusterID_eventID endpointID clusterID eventID =
  do
    cls' <- getRequiredClass "MTREventRequestPath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        withObjCPtr eventID $ \raw_eventID ->
          sendClassMsg cls' (mkSelector "requestPathWithEndpointID:clusterID:eventID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_eventID :: Ptr ())] >>= retainedObject . castPtr

-- | @- endpoint@
endpoint :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
endpoint mtrEventRequestPath  =
    sendMsg mtrEventRequestPath (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cluster@
cluster :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
cluster mtrEventRequestPath  =
    sendMsg mtrEventRequestPath (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- event@
event :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
event mtrEventRequestPath  =
    sendMsg mtrEventRequestPath (mkSelector "event") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestPathWithEndpointID:clusterID:eventID:@
requestPathWithEndpointID_clusterID_eventIDSelector :: Selector
requestPathWithEndpointID_clusterID_eventIDSelector = mkSelector "requestPathWithEndpointID:clusterID:eventID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

