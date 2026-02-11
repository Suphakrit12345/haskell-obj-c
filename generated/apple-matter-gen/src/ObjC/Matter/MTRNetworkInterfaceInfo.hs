{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a network interface on a Matter node.
--
-- Generated bindings for @MTRNetworkInterfaceInfo@.
module ObjC.Matter.MTRNetworkInterfaceInfo
  ( MTRNetworkInterfaceInfo
  , IsMTRNetworkInterfaceInfo(..)
  , init_
  , new
  , endpointID
  , featureMap
  , type_
  , initSelector
  , newSelector
  , endpointIDSelector
  , featureMapSelector
  , typeSelector

  -- * Enum types
  , MTRNetworkCommissioningFeature(MTRNetworkCommissioningFeature)
  , pattern MTRNetworkCommissioningFeatureWiFiNetworkInterface
  , pattern MTRNetworkCommissioningFeatureThreadNetworkInterface
  , pattern MTRNetworkCommissioningFeatureEthernetNetworkInterface
  , pattern MTRNetworkCommissioningFeaturePerDeviceCredentials

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id MTRNetworkInterfaceInfo)
init_ mtrNetworkInterfaceInfo  =
    sendMsg mtrNetworkInterfaceInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRNetworkInterfaceInfo)
new  =
  do
    cls' <- getRequiredClass "MTRNetworkInterfaceInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The endpoint this network interface is exposed on (i.e. the endpoint its corresponding Network Commissioning server cluster instance is on).
--
-- ObjC selector: @- endpointID@
endpointID :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id NSNumber)
endpointID mtrNetworkInterfaceInfo  =
    sendMsg mtrNetworkInterfaceInfo (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the FeatureMap attribute of the corresponding Network Commissioning cluster instance.
--
-- ObjC selector: @- featureMap@
featureMap :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id NSNumber)
featureMap mtrNetworkInterfaceInfo  =
    sendMsg mtrNetworkInterfaceInfo (mkSelector "featureMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The network technology used by the interface.  This will be one of the MTRNetworkCommissioningFeature*NetworkInterface values (exactly one bit).
--
-- ObjC selector: @- type@
type_ :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO MTRNetworkCommissioningFeature
type_ mtrNetworkInterfaceInfo  =
    fmap (coerce :: CUInt -> MTRNetworkCommissioningFeature) $ sendMsg mtrNetworkInterfaceInfo (mkSelector "type") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @featureMap@
featureMapSelector :: Selector
featureMapSelector = mkSelector "featureMap"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

