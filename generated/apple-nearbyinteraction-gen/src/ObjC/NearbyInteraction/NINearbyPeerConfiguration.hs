{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to describe and configure parameters to be used in a nearby interaction session for mutual relative positional measurements.
--
-- Devices engaged in a session run with an NINearbyPeerConfiguration are able to continuously generate positional measurements relative to one another.
--
-- Generated bindings for @NINearbyPeerConfiguration@.
module ObjC.NearbyInteraction.NINearbyPeerConfiguration
  ( NINearbyPeerConfiguration
  , IsNINearbyPeerConfiguration(..)
  , initWithPeerToken
  , init_
  , new
  , peerDiscoveryToken
  , cameraAssistanceEnabled
  , setCameraAssistanceEnabled
  , extendedDistanceMeasurementEnabled
  , setExtendedDistanceMeasurementEnabled
  , initWithPeerTokenSelector
  , initSelector
  , newSelector
  , peerDiscoveryTokenSelector
  , cameraAssistanceEnabledSelector
  , setCameraAssistanceEnabledSelector
  , extendedDistanceMeasurementEnabledSelector
  , setExtendedDistanceMeasurementEnabledSelector


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

-- | Initializes a new configuration with the provided peer token.
--
-- @peerToken@ — A discovery token received from the peer for this session.
--
-- ObjC selector: @- initWithPeerToken:@
initWithPeerToken :: (IsNINearbyPeerConfiguration niNearbyPeerConfiguration, IsNIDiscoveryToken peerToken) => niNearbyPeerConfiguration -> peerToken -> IO (Id NINearbyPeerConfiguration)
initWithPeerToken niNearbyPeerConfiguration  peerToken =
  withObjCPtr peerToken $ \raw_peerToken ->
      sendMsg niNearbyPeerConfiguration (mkSelector "initWithPeerToken:") (retPtr retVoid) [argPtr (castPtr raw_peerToken :: Ptr ())] >>= ownedObject . castPtr

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO (Id NINearbyPeerConfiguration)
init_ niNearbyPeerConfiguration  =
    sendMsg niNearbyPeerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NINearbyPeerConfiguration)
new  =
  do
    cls' <- getRequiredClass "NINearbyPeerConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The discovery token identifying the peer device for this session configuration.
--
-- ObjC selector: @- peerDiscoveryToken@
peerDiscoveryToken :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO (Id NIDiscoveryToken)
peerDiscoveryToken niNearbyPeerConfiguration  =
    sendMsg niNearbyPeerConfiguration (mkSelector "peerDiscoveryToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If true, optionally call setARSession: on the NISession before calling runWithConfiguration: If true and setARSession: is not called, an ARSession will automatically be created If true and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- cameraAssistanceEnabled@
cameraAssistanceEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO Bool
cameraAssistanceEnabled niNearbyPeerConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg niNearbyPeerConfiguration (mkSelector "cameraAssistanceEnabled") retCULong []

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If true, optionally call setARSession: on the NISession before calling runWithConfiguration: If true and setARSession: is not called, an ARSession will automatically be created If true and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- setCameraAssistanceEnabled:@
setCameraAssistanceEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> Bool -> IO ()
setCameraAssistanceEnabled niNearbyPeerConfiguration  value =
    sendMsg niNearbyPeerConfiguration (mkSelector "setCameraAssistanceEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | If both peers are capable, enables extended distance measurement for the NISession that runs with this configuration
--
-- :If true, the NISession will use extended distance measurement capabilities while ranging with a peer that is also capable of extended distance measurementThis property is compatible with the cameraAssistanceEnabled property
--
-- Note: : Check supportsExtendedDistanceMeasurement property from deviceCapabilities properties on NISession and the deviceCapabilities property on the NIDiscoveryToken generated by the peer device to understand mutual capabilities
--
-- ObjC selector: @- extendedDistanceMeasurementEnabled@
extendedDistanceMeasurementEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO Bool
extendedDistanceMeasurementEnabled niNearbyPeerConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg niNearbyPeerConfiguration (mkSelector "extendedDistanceMeasurementEnabled") retCULong []

-- | If both peers are capable, enables extended distance measurement for the NISession that runs with this configuration
--
-- :If true, the NISession will use extended distance measurement capabilities while ranging with a peer that is also capable of extended distance measurementThis property is compatible with the cameraAssistanceEnabled property
--
-- Note: : Check supportsExtendedDistanceMeasurement property from deviceCapabilities properties on NISession and the deviceCapabilities property on the NIDiscoveryToken generated by the peer device to understand mutual capabilities
--
-- ObjC selector: @- setExtendedDistanceMeasurementEnabled:@
setExtendedDistanceMeasurementEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> Bool -> IO ()
setExtendedDistanceMeasurementEnabled niNearbyPeerConfiguration  value =
    sendMsg niNearbyPeerConfiguration (mkSelector "setExtendedDistanceMeasurementEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeerToken:@
initWithPeerTokenSelector :: Selector
initWithPeerTokenSelector = mkSelector "initWithPeerToken:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @peerDiscoveryToken@
peerDiscoveryTokenSelector :: Selector
peerDiscoveryTokenSelector = mkSelector "peerDiscoveryToken"

-- | @Selector@ for @cameraAssistanceEnabled@
cameraAssistanceEnabledSelector :: Selector
cameraAssistanceEnabledSelector = mkSelector "cameraAssistanceEnabled"

-- | @Selector@ for @setCameraAssistanceEnabled:@
setCameraAssistanceEnabledSelector :: Selector
setCameraAssistanceEnabledSelector = mkSelector "setCameraAssistanceEnabled:"

-- | @Selector@ for @extendedDistanceMeasurementEnabled@
extendedDistanceMeasurementEnabledSelector :: Selector
extendedDistanceMeasurementEnabledSelector = mkSelector "extendedDistanceMeasurementEnabled"

-- | @Selector@ for @setExtendedDistanceMeasurementEnabled:@
setExtendedDistanceMeasurementEnabledSelector :: Selector
setExtendedDistanceMeasurementEnabledSelector = mkSelector "setExtendedDistanceMeasurementEnabled:"

