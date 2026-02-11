{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASMigrationDisplayItem@.
module ObjC.AccessorySetupKit.ASMigrationDisplayItem
  ( ASMigrationDisplayItem
  , IsASMigrationDisplayItem(..)
  , peripheralIdentifier
  , setPeripheralIdentifier
  , hotspotSSID
  , setHotspotSSID
  , wifiAwarePairedDeviceID
  , setWifiAwarePairedDeviceID
  , peripheralIdentifierSelector
  , setPeripheralIdentifierSelector
  , hotspotSSIDSelector
  , setHotspotSSIDSelector
  , wifiAwarePairedDeviceIDSelector
  , setWifiAwarePairedDeviceIDSelector


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

-- | The Bluetooth identifier of the accessory to migrate.
--
-- ObjC selector: @- peripheralIdentifier@
peripheralIdentifier :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO (Id NSUUID)
peripheralIdentifier asMigrationDisplayItem  =
    sendMsg asMigrationDisplayItem (mkSelector "peripheralIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Bluetooth identifier of the accessory to migrate.
--
-- ObjC selector: @- setPeripheralIdentifier:@
setPeripheralIdentifier :: (IsASMigrationDisplayItem asMigrationDisplayItem, IsNSUUID value) => asMigrationDisplayItem -> value -> IO ()
setPeripheralIdentifier asMigrationDisplayItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asMigrationDisplayItem (mkSelector "setPeripheralIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Wi-Fi hotspot SSID of the accessory to migrate.
--
-- ObjC selector: @- hotspotSSID@
hotspotSSID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO (Id NSString)
hotspotSSID asMigrationDisplayItem  =
    sendMsg asMigrationDisplayItem (mkSelector "hotspotSSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Wi-Fi hotspot SSID of the accessory to migrate.
--
-- ObjC selector: @- setHotspotSSID:@
setHotspotSSID :: (IsASMigrationDisplayItem asMigrationDisplayItem, IsNSString value) => asMigrationDisplayItem -> value -> IO ()
setHotspotSSID asMigrationDisplayItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asMigrationDisplayItem (mkSelector "setHotspotSSID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Wi-Fi Aware paired device identififer of the accessory to migrate.
--
-- ObjC selector: @- wifiAwarePairedDeviceID@
wifiAwarePairedDeviceID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO CULong
wifiAwarePairedDeviceID asMigrationDisplayItem  =
    sendMsg asMigrationDisplayItem (mkSelector "wifiAwarePairedDeviceID") retCULong []

-- | The Wi-Fi Aware paired device identififer of the accessory to migrate.
--
-- ObjC selector: @- setWifiAwarePairedDeviceID:@
setWifiAwarePairedDeviceID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> CULong -> IO ()
setWifiAwarePairedDeviceID asMigrationDisplayItem  value =
    sendMsg asMigrationDisplayItem (mkSelector "setWifiAwarePairedDeviceID:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peripheralIdentifier@
peripheralIdentifierSelector :: Selector
peripheralIdentifierSelector = mkSelector "peripheralIdentifier"

-- | @Selector@ for @setPeripheralIdentifier:@
setPeripheralIdentifierSelector :: Selector
setPeripheralIdentifierSelector = mkSelector "setPeripheralIdentifier:"

-- | @Selector@ for @hotspotSSID@
hotspotSSIDSelector :: Selector
hotspotSSIDSelector = mkSelector "hotspotSSID"

-- | @Selector@ for @setHotspotSSID:@
setHotspotSSIDSelector :: Selector
setHotspotSSIDSelector = mkSelector "setHotspotSSID:"

-- | @Selector@ for @wifiAwarePairedDeviceID@
wifiAwarePairedDeviceIDSelector :: Selector
wifiAwarePairedDeviceIDSelector = mkSelector "wifiAwarePairedDeviceID"

-- | @Selector@ for @setWifiAwarePairedDeviceID:@
setWifiAwarePairedDeviceIDSelector :: Selector
setWifiAwarePairedDeviceIDSelector = mkSelector "setWifiAwarePairedDeviceID:"

