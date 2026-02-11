{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccessory@.
module ObjC.AccessorySetupKit.ASAccessory
  ( ASAccessory
  , IsASAccessory(..)
  , state
  , bluetoothIdentifier
  , bluetoothTransportBridgingIdentifier
  , displayName
  , ssid
  , wifiAwarePairedDeviceID
  , descriptor
  , stateSelector
  , bluetoothIdentifierSelector
  , bluetoothTransportBridgingIdentifierSelector
  , displayNameSelector
  , ssidSelector
  , wifiAwarePairedDeviceIDSelector
  , descriptorSelector

  -- * Enum types
  , ASAccessoryState(ASAccessoryState)
  , pattern ASAccessoryStateUnauthorized
  , pattern ASAccessoryStateAwaitingAuthorization
  , pattern ASAccessoryStateAuthorized

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
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The current authorization state of the accessory.
--
-- ObjC selector: @- state@
state :: IsASAccessory asAccessory => asAccessory -> IO ASAccessoryState
state asAccessory  =
    fmap (coerce :: CLong -> ASAccessoryState) $ sendMsg asAccessory (mkSelector "state") retCLong []

-- | The accessory's unique Bluetooth identifier, if any.
--
-- Use this identifier to establish a connection to the accessory.
--
-- ObjC selector: @- bluetoothIdentifier@
bluetoothIdentifier :: IsASAccessory asAccessory => asAccessory -> IO (Id NSUUID)
bluetoothIdentifier asAccessory  =
    sendMsg asAccessory (mkSelector "bluetoothIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Bluetooth identifier, if any, for use when bridging classic transport profiles.
--
-- ObjC selector: @- bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifier :: IsASAccessory asAccessory => asAccessory -> IO (Id NSData)
bluetoothTransportBridgingIdentifier asAccessory  =
    sendMsg asAccessory (mkSelector "bluetoothTransportBridgingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's name, suitable for displaying to someone using your app.
--
-- ObjC selector: @- displayName@
displayName :: IsASAccessory asAccessory => asAccessory -> IO (Id NSString)
displayName asAccessory  =
    sendMsg asAccessory (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Wi-Fi SSID, if any.
--
-- Use this identifier to establish a connection to the accessory.
--
-- ObjC selector: @- SSID@
ssid :: IsASAccessory asAccessory => asAccessory -> IO (Id NSString)
ssid asAccessory  =
    sendMsg asAccessory (mkSelector "SSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Wi-Fi Aware Pairing Identifier.
--
-- Use this identifier to establish a connection to the accessory using Wi-Fi Aware Framework.
--
-- ObjC selector: @- wifiAwarePairedDeviceID@
wifiAwarePairedDeviceID :: IsASAccessory asAccessory => asAccessory -> IO CULong
wifiAwarePairedDeviceID asAccessory  =
    sendMsg asAccessory (mkSelector "wifiAwarePairedDeviceID") retCULong []

-- | The descriptor used to discover the accessory.
--
-- ObjC selector: @- descriptor@
descriptor :: IsASAccessory asAccessory => asAccessory -> IO (Id ASDiscoveryDescriptor)
descriptor asAccessory  =
    sendMsg asAccessory (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifierSelector :: Selector
bluetoothTransportBridgingIdentifierSelector = mkSelector "bluetoothTransportBridgingIdentifier"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @SSID@
ssidSelector :: Selector
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @wifiAwarePairedDeviceID@
wifiAwarePairedDeviceIDSelector :: Selector
wifiAwarePairedDeviceIDSelector = mkSelector "wifiAwarePairedDeviceID"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

