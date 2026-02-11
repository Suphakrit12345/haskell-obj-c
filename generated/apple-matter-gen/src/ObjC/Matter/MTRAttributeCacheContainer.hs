{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeCacheContainer@.
module ObjC.Matter.MTRAttributeCacheContainer
  ( MTRAttributeCacheContainer
  , IsMTRAttributeCacheContainer(..)
  , readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion
  , readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector


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

-- | @- readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion :: (IsMTRAttributeCacheContainer mtrAttributeCacheContainer, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId, IsNSObject clientQueue) => mtrAttributeCacheContainer -> endpointId -> clusterId -> attributeId -> clientQueue -> Ptr () -> IO ()
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion mtrAttributeCacheContainer  endpointId clusterId attributeId clientQueue completion =
  withObjCPtr endpointId $ \raw_endpointId ->
    withObjCPtr clusterId $ \raw_clusterId ->
      withObjCPtr attributeId $ \raw_attributeId ->
        withObjCPtr clientQueue $ \raw_clientQueue ->
            sendMsg mtrAttributeCacheContainer (mkSelector "readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:") retVoid [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_attributeId :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector :: Selector
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector = mkSelector "readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:"

