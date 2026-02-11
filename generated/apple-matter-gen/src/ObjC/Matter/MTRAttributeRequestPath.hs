{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating an attribute being requested (for read or subscribe).
--
-- nil is used to represent wildcards.
--
-- Generated bindings for @MTRAttributeRequestPath@.
module ObjC.Matter.MTRAttributeRequestPath
  ( MTRAttributeRequestPath
  , IsMTRAttributeRequestPath(..)
  , requestPathWithEndpointID_clusterID_attributeID
  , endpoint
  , cluster
  , attribute
  , requestPathWithEndpointID_clusterID_attributeIDSelector
  , endpointSelector
  , clusterSelector
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

-- | @+ requestPathWithEndpointID:clusterID:attributeID:@
requestPathWithEndpointID_clusterID_attributeID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID) => endpointID -> clusterID -> attributeID -> IO (Id MTRAttributeRequestPath)
requestPathWithEndpointID_clusterID_attributeID endpointID clusterID attributeID =
  do
    cls' <- getRequiredClass "MTRAttributeRequestPath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        withObjCPtr attributeID $ \raw_attributeID ->
          sendClassMsg cls' (mkSelector "requestPathWithEndpointID:clusterID:attributeID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ())] >>= retainedObject . castPtr

-- | @- endpoint@
endpoint :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
endpoint mtrAttributeRequestPath  =
    sendMsg mtrAttributeRequestPath (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cluster@
cluster :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
cluster mtrAttributeRequestPath  =
    sendMsg mtrAttributeRequestPath (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attribute@
attribute :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
attribute mtrAttributeRequestPath  =
    sendMsg mtrAttributeRequestPath (mkSelector "attribute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestPathWithEndpointID:clusterID:attributeID:@
requestPathWithEndpointID_clusterID_attributeIDSelector :: Selector
requestPathWithEndpointID_clusterID_attributeIDSelector = mkSelector "requestPathWithEndpointID:clusterID:attributeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

