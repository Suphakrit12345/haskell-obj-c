{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterWiFiInterfaceScanResult@.
module ObjC.Matter.MTRNetworkCommissioningClusterWiFiInterfaceScanResult
  ( MTRNetworkCommissioningClusterWiFiInterfaceScanResult
  , IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult(..)
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
security :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
security mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "security") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecurity:@
setSecurity :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setSecurity mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setSecurity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "ssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setSsid mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setSsid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bssid@
bssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSData)
bssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "bssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBssid:@
setBssid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setBssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setBssid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setChannel mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wiFiBand@
wiFiBand :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
wiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "wiFiBand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWiFiBand:@
setWiFiBand :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setWiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setWiFiBand:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterWiFiInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "rssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setRssi mtrNetworkCommissioningClusterWiFiInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterWiFiInterfaceScanResult (mkSelector "setRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

