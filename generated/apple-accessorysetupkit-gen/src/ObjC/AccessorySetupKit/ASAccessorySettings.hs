{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccessorySettings@.
module ObjC.AccessorySetupKit.ASAccessorySettings
  ( ASAccessorySettings
  , IsASAccessorySettings(..)
  , defaultSettings
  , ssid
  , setSSID
  , bluetoothTransportBridgingIdentifier
  , setBluetoothTransportBridgingIdentifier
  , defaultSettingsSelector
  , ssidSelector
  , setSSIDSelector
  , bluetoothTransportBridgingIdentifierSelector
  , setBluetoothTransportBridgingIdentifierSelector


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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An empty settings object.
--
-- ObjC selector: @+ defaultSettings@
defaultSettings :: IO (Id ASAccessorySettings)
defaultSettings  =
  do
    cls' <- getRequiredClass "ASAccessorySettings"
    sendClassMsg cls' (mkSelector "defaultSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A hotspot identifier that clients can use to connect to an accessory's hotspot.
--
-- ObjC selector: @- SSID@
ssid :: IsASAccessorySettings asAccessorySettings => asAccessorySettings -> IO (Id NSString)
ssid asAccessorySettings  =
    sendMsg asAccessorySettings (mkSelector "SSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A hotspot identifier that clients can use to connect to an accessory's hotspot.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsASAccessorySettings asAccessorySettings, IsNSString value) => asAccessorySettings -> value -> IO ()
setSSID asAccessorySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAccessorySettings (mkSelector "setSSID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A 6-byte identifier for bridging classic transport profiles.
--
-- AccessorySetupKit ignores this property if another app already authorized and bridged the accessory.
--
-- ObjC selector: @- bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifier :: IsASAccessorySettings asAccessorySettings => asAccessorySettings -> IO (Id NSData)
bluetoothTransportBridgingIdentifier asAccessorySettings  =
    sendMsg asAccessorySettings (mkSelector "bluetoothTransportBridgingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A 6-byte identifier for bridging classic transport profiles.
--
-- AccessorySetupKit ignores this property if another app already authorized and bridged the accessory.
--
-- ObjC selector: @- setBluetoothTransportBridgingIdentifier:@
setBluetoothTransportBridgingIdentifier :: (IsASAccessorySettings asAccessorySettings, IsNSData value) => asAccessorySettings -> value -> IO ()
setBluetoothTransportBridgingIdentifier asAccessorySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAccessorySettings (mkSelector "setBluetoothTransportBridgingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSettings@
defaultSettingsSelector :: Selector
defaultSettingsSelector = mkSelector "defaultSettings"

-- | @Selector@ for @SSID@
ssidSelector :: Selector
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifierSelector :: Selector
bluetoothTransportBridgingIdentifierSelector = mkSelector "bluetoothTransportBridgingIdentifier"

-- | @Selector@ for @setBluetoothTransportBridgingIdentifier:@
setBluetoothTransportBridgingIdentifierSelector :: Selector
setBluetoothTransportBridgingIdentifierSelector = mkSelector "setBluetoothTransportBridgingIdentifier:"

