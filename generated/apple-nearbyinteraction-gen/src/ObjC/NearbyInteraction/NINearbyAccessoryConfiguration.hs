{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A session configuration that enables interaction with supported accessories.
--
-- Generated bindings for @NINearbyAccessoryConfiguration@.
module ObjC.NearbyInteraction.NINearbyAccessoryConfiguration
  ( NINearbyAccessoryConfiguration
  , IsNINearbyAccessoryConfiguration(..)
  , initWithData_error
  , initWithAccessoryData_bluetoothPeerIdentifier_error
  , init_
  , new
  , accessoryDiscoveryToken
  , cameraAssistanceEnabled
  , setCameraAssistanceEnabled
  , initWithData_errorSelector
  , initWithAccessoryData_bluetoothPeerIdentifier_errorSelector
  , initSelector
  , newSelector
  , accessoryDiscoveryTokenSelector
  , cameraAssistanceEnabledSelector
  , setCameraAssistanceEnabledSelector


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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new nearby accessory configuration using data received from the accessory.
--
-- @data@ — Configuration data received from the accessory.
--
-- @error@ — An optional out error parameter that will be populated with an error if the provided data is invalid or unsupported.
--
-- ObjC selector: @- initWithData:error:@
initWithData_error :: (IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration, IsNSData data_, IsNSError error_) => niNearbyAccessoryConfiguration -> data_ -> error_ -> IO (Id NINearbyAccessoryConfiguration)
initWithData_error niNearbyAccessoryConfiguration  data_ error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg niNearbyAccessoryConfiguration (mkSelector "initWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new nearby accessory configuration for an accessory that is also a paired Bluetooth device
--
-- @accessoryData@ — Configuration data received from the accessory
--
-- @bluetoothPeerIdentifier@ — The accessory's Bluetooth identifier
--
-- @error@ — An optional out error parameter that will be populated with an error if the provided inputs are invalid or unsupported.
--
-- The accessory must be a Bluetooth LE peripheral that is paired, actively connected, and implements the Nearby Interaction Service and Accessory Configuration Characteristic.
--
-- ObjC selector: @- initWithAccessoryData:bluetoothPeerIdentifier:error:@
initWithAccessoryData_bluetoothPeerIdentifier_error :: (IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration, IsNSData accessoryData, IsNSUUID identifier, IsNSError error_) => niNearbyAccessoryConfiguration -> accessoryData -> identifier -> error_ -> IO (Id NINearbyAccessoryConfiguration)
initWithAccessoryData_bluetoothPeerIdentifier_error niNearbyAccessoryConfiguration  accessoryData identifier error_ =
  withObjCPtr accessoryData $ \raw_accessoryData ->
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg niNearbyAccessoryConfiguration (mkSelector "initWithAccessoryData:bluetoothPeerIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_accessoryData :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO (Id NINearbyAccessoryConfiguration)
init_ niNearbyAccessoryConfiguration  =
    sendMsg niNearbyAccessoryConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NINearbyAccessoryConfiguration)
new  =
  do
    cls' <- getRequiredClass "NINearbyAccessoryConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The discovery token identifying the accessory device for this session configuration.
--
-- NINearbyObject updates for this accessory will contain this discovery token.
--
-- ObjC selector: @- accessoryDiscoveryToken@
accessoryDiscoveryToken :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO (Id NIDiscoveryToken)
accessoryDiscoveryToken niNearbyAccessoryConfiguration  =
    sendMsg niNearbyAccessoryConfiguration (mkSelector "accessoryDiscoveryToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If YES, optionally call -setARSession: on the NISession before calling -runWithConfiguration: If YES and setARSession: is not called, an ARSession will automatically be created If YES  and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- cameraAssistanceEnabled@
cameraAssistanceEnabled :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO Bool
cameraAssistanceEnabled niNearbyAccessoryConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg niNearbyAccessoryConfiguration (mkSelector "cameraAssistanceEnabled") retCULong []

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If YES, optionally call -setARSession: on the NISession before calling -runWithConfiguration: If YES and setARSession: is not called, an ARSession will automatically be created If YES  and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- setCameraAssistanceEnabled:@
setCameraAssistanceEnabled :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> Bool -> IO ()
setCameraAssistanceEnabled niNearbyAccessoryConfiguration  value =
    sendMsg niNearbyAccessoryConfiguration (mkSelector "setCameraAssistanceEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithAccessoryData:bluetoothPeerIdentifier:error:@
initWithAccessoryData_bluetoothPeerIdentifier_errorSelector :: Selector
initWithAccessoryData_bluetoothPeerIdentifier_errorSelector = mkSelector "initWithAccessoryData:bluetoothPeerIdentifier:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @accessoryDiscoveryToken@
accessoryDiscoveryTokenSelector :: Selector
accessoryDiscoveryTokenSelector = mkSelector "accessoryDiscoveryToken"

-- | @Selector@ for @cameraAssistanceEnabled@
cameraAssistanceEnabledSelector :: Selector
cameraAssistanceEnabledSelector = mkSelector "cameraAssistanceEnabled"

-- | @Selector@ for @setCameraAssistanceEnabled:@
setCameraAssistanceEnabledSelector :: Selector
setCameraAssistanceEnabledSelector = mkSelector "setCameraAssistanceEnabled:"

