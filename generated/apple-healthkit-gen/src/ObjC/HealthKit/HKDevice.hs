{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKDevice@.
module ObjC.HealthKit.HKDevice
  ( HKDevice
  , IsHKDevice(..)
  , initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier
  , init_
  , localDevice
  , name
  , manufacturer
  , model
  , hardwareVersion
  , firmwareVersion
  , softwareVersion
  , localIdentifier
  , udiDeviceIdentifier
  , initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector
  , initSelector
  , localDeviceSelector
  , nameSelector
  , manufacturerSelector
  , modelSelector
  , hardwareVersionSelector
  , firmwareVersionSelector
  , softwareVersionSelector
  , localIdentifierSelector
  , udiDeviceIdentifierSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:
--
-- Initialize a new HKDevice with the specified values.
--
-- This allows initialization of an HKDevice object based on the                information provided.
--
-- ObjC selector: @- initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:@
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier :: (IsHKDevice hkDevice, IsNSString name, IsNSString manufacturer, IsNSString model, IsNSString hardwareVersion, IsNSString firmwareVersion, IsNSString softwareVersion, IsNSString localIdentifier, IsNSString udiDeviceIdentifier) => hkDevice -> name -> manufacturer -> model -> hardwareVersion -> firmwareVersion -> softwareVersion -> localIdentifier -> udiDeviceIdentifier -> IO (Id HKDevice)
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier hkDevice  name manufacturer model hardwareVersion firmwareVersion softwareVersion localIdentifier udiDeviceIdentifier =
  withObjCPtr name $ \raw_name ->
    withObjCPtr manufacturer $ \raw_manufacturer ->
      withObjCPtr model $ \raw_model ->
        withObjCPtr hardwareVersion $ \raw_hardwareVersion ->
          withObjCPtr firmwareVersion $ \raw_firmwareVersion ->
            withObjCPtr softwareVersion $ \raw_softwareVersion ->
              withObjCPtr localIdentifier $ \raw_localIdentifier ->
                withObjCPtr udiDeviceIdentifier $ \raw_udiDeviceIdentifier ->
                    sendMsg hkDevice (mkSelector "initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_manufacturer :: Ptr ()), argPtr (castPtr raw_model :: Ptr ()), argPtr (castPtr raw_hardwareVersion :: Ptr ()), argPtr (castPtr raw_firmwareVersion :: Ptr ()), argPtr (castPtr raw_softwareVersion :: Ptr ()), argPtr (castPtr raw_localIdentifier :: Ptr ()), argPtr (castPtr raw_udiDeviceIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKDevice hkDevice => hkDevice -> IO (Id HKDevice)
init_ hkDevice  =
    sendMsg hkDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | localDevice
--
-- Returns a device representing the host.
--
-- If an app chooses to save samples that were retrieved from the local device, e.g. an HKWorkout with a                 totalDistance HKQuantity gathered from CoreLocation GPS distances, then this would be an appropriate                 HKDevice to use.
--
-- ObjC selector: @+ localDevice@
localDevice :: IO (Id HKDevice)
localDevice  =
  do
    cls' <- getRequiredClass "HKDevice"
    sendClassMsg cls' (mkSelector "localDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- The name of the receiver.
--
-- The user-facing name, such as the one displayed in the Bluetooth Settings for a BLE device.
--
-- ObjC selector: @- name@
name :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
name hkDevice  =
    sendMsg hkDevice (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | manufacturer
--
-- The manufacturer of the receiver.
--
-- ObjC selector: @- manufacturer@
manufacturer :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
manufacturer hkDevice  =
    sendMsg hkDevice (mkSelector "manufacturer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | model
--
-- The model of the receiver.
--
-- ObjC selector: @- model@
model :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
model hkDevice  =
    sendMsg hkDevice (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hardwareVersion
--
-- The hardware revision of the receiver.
--
-- ObjC selector: @- hardwareVersion@
hardwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
hardwareVersion hkDevice  =
    sendMsg hkDevice (mkSelector "hardwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | firmwareVersion
--
-- The firmware revision of the receiver.
--
-- ObjC selector: @- firmwareVersion@
firmwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
firmwareVersion hkDevice  =
    sendMsg hkDevice (mkSelector "firmwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | softwareVersion
--
-- The software revision of the receiver.
--
-- ObjC selector: @- softwareVersion@
softwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
softwareVersion hkDevice  =
    sendMsg hkDevice (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localIdentifier
--
-- A unique identifier for the receiver.
--
-- This property is available to clients for a local identifier.                For example, Bluetooth peripherals managed by HealthKit use this                for the CoreBluetooth UUID which is valid only on the local                device and thus distinguish the same Bluetooth peripheral used                between multiple devices.
--
-- ObjC selector: @- localIdentifier@
localIdentifier :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
localIdentifier hkDevice  =
    sendMsg hkDevice (mkSelector "localIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | UDIDeviceIdentifier
--
-- Represents the device identifier portion of a device's FDA UDI (Unique Device Identifier).
--
-- The device identifier can be used to reference the FDA's GUDID (Globally Unique Device                Identifier Database). Note that for user privacy concerns this field should not be used to                persist the production identifier portion of the device UDI. HealthKit clients should manage                the production identifier independently, if needed.                See http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/ for more information.
--
-- ObjC selector: @- UDIDeviceIdentifier@
udiDeviceIdentifier :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
udiDeviceIdentifier hkDevice  =
    sendMsg hkDevice (mkSelector "UDIDeviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:@
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector :: Selector
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector = mkSelector "initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @localDevice@
localDeviceSelector :: Selector
localDeviceSelector = mkSelector "localDevice"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @hardwareVersion@
hardwareVersionSelector :: Selector
hardwareVersionSelector = mkSelector "hardwareVersion"

-- | @Selector@ for @firmwareVersion@
firmwareVersionSelector :: Selector
firmwareVersionSelector = mkSelector "firmwareVersion"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @UDIDeviceIdentifier@
udiDeviceIdentifierSelector :: Selector
udiDeviceIdentifierSelector = mkSelector "UDIDeviceIdentifier"

