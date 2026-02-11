{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateRequestorClusterProviderLocation@.
module ObjC.Matter.MTROtaSoftwareUpdateRequestorClusterProviderLocation
  ( MTROtaSoftwareUpdateRequestorClusterProviderLocation
  , IsMTROtaSoftwareUpdateRequestorClusterProviderLocation(..)
  , providerNodeID
  , setProviderNodeID
  , endpoint
  , setEndpoint
  , fabricIndex
  , setFabricIndex
  , providerNodeIDSelector
  , setProviderNodeIDSelector
  , endpointSelector
  , setEndpointSelector
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

-- | @- providerNodeID@
providerNodeID :: IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
providerNodeID mtrOtaSoftwareUpdateRequestorClusterProviderLocation  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "providerNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProviderNodeID:@
setProviderNodeID :: (IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setProviderNodeID mtrOtaSoftwareUpdateRequestorClusterProviderLocation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "setProviderNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
endpoint mtrOtaSoftwareUpdateRequestorClusterProviderLocation  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setEndpoint mtrOtaSoftwareUpdateRequestorClusterProviderLocation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
fabricIndex mtrOtaSoftwareUpdateRequestorClusterProviderLocation  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROtaSoftwareUpdateRequestorClusterProviderLocation mtrOtaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setFabricIndex mtrOtaSoftwareUpdateRequestorClusterProviderLocation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterProviderLocation (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerNodeID@
providerNodeIDSelector :: Selector
providerNodeIDSelector = mkSelector "providerNodeID"

-- | @Selector@ for @setProviderNodeID:@
setProviderNodeIDSelector :: Selector
setProviderNodeIDSelector = mkSelector "setProviderNodeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

