{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct
  ( MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct
  , IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct(..)
  , security
  , setSecurity
  , ssid
  , setSsid
  , bssid
  , setBssid
  , channel
  , setChannel
  , wiFiBand
  , setWiFiBand
  , rssi
  , setRssi
  , securitySelector
  , setSecuritySelector
  , ssidSelector
  , setSsidSelector
  , bssidSelector
  , setBssidSelector
  , channelSelector
  , setChannelSelector
  , wiFiBandSelector
  , setWiFiBandSelector
  , rssiSelector
  , setRssiSelector


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

-- | @- security@
security :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
security mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "security") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecurity:@
setSecurity :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setSecurity mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setSecurity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "ssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setSsid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setSsid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bssid@
bssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSData)
bssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "bssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBssid:@
setBssid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setBssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setBssid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setChannel mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wiFiBand@
wiFiBand :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
wiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "wiFiBand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWiFiBand:@
setWiFiBand :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setWiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setWiFiBand:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "rssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setRssi mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct (mkSelector "setRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @security@
securitySelector :: Selector
securitySelector = mkSelector "security"

-- | @Selector@ for @setSecurity:@
setSecuritySelector :: Selector
setSecuritySelector = mkSelector "setSecurity:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @setSsid:@
setSsidSelector :: Selector
setSsidSelector = mkSelector "setSsid:"

-- | @Selector@ for @bssid@
bssidSelector :: Selector
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @setBssid:@
setBssidSelector :: Selector
setBssidSelector = mkSelector "setBssid:"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @wiFiBand@
wiFiBandSelector :: Selector
wiFiBandSelector = mkSelector "wiFiBand"

-- | @Selector@ for @setWiFiBand:@
setWiFiBandSelector :: Selector
setWiFiBandSelector = mkSelector "setWiFiBand:"

-- | @Selector@ for @rssi@
rssiSelector :: Selector
rssiSelector = mkSelector "rssi"

-- | @Selector@ for @setRssi:@
setRssiSelector :: Selector
setRssiSelector = mkSelector "setRssi:"

