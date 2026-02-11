{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotConfigurationManager
--
-- The NEHotspotConfigurationManager class allows an application to   Add/Update/Remove Wi-Fi Network Configuraton.
--
-- Generated bindings for @NEHotspotConfigurationManager@.
module ObjC.NetworkExtension.NEHotspotConfigurationManager
  ( NEHotspotConfigurationManager
  , IsNEHotspotConfigurationManager(..)
  , applyConfiguration_completionHandler
  , removeConfigurationForSSID
  , removeConfigurationForHS20DomainName
  , joinAccessoryHotspot_passphrase_completionHandler
  , joinAccessoryHotspotWithoutSecurity_completionHandler
  , sharedManager
  , applyConfiguration_completionHandlerSelector
  , removeConfigurationForSSIDSelector
  , removeConfigurationForHS20DomainNameSelector
  , joinAccessoryHotspot_passphrase_completionHandlerSelector
  , joinAccessoryHotspotWithoutSecurity_completionHandlerSelector
  , sharedManagerSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | applyConfiguration:
--
-- This function adds or updates a Wi-Fi network configuration.
--
-- @configuration@ — NEHotspotConfiguration object containing the Wi-Fi network configuration.
--
-- @completionHandler@ — A block that will be called when add/update operation is completed.   Pass nil if application does not intend to receive the result.   The NSError passed to this block will be nil if the configuration is successfully stored, non-nil otherwise.   If the configuration is found invalid or API encounters some other error then completionHandler is called   with instance of NSError containing appropriate error code. This API attempts to join the Wi-Fi network   if the configuration is successfully added or updated and the network is found nearby.
--
-- ObjC selector: @- applyConfiguration:completionHandler:@
applyConfiguration_completionHandler :: (IsNEHotspotConfigurationManager neHotspotConfigurationManager, IsNEHotspotConfiguration configuration) => neHotspotConfigurationManager -> configuration -> Ptr () -> IO ()
applyConfiguration_completionHandler neHotspotConfigurationManager  configuration completionHandler =
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg neHotspotConfigurationManager (mkSelector "applyConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | removeConfigurationForSSID:
--
-- This function removes Wi-Fi configuration.   If the joinOnce property was set to YES, invoking this method will disassociate from the Wi-Fi network   after the configuration is removed.
--
-- @SSID@ — Wi-Fi SSID for which the configuration is to be deleted.
--
-- ObjC selector: @- removeConfigurationForSSID:@
removeConfigurationForSSID :: (IsNEHotspotConfigurationManager neHotspotConfigurationManager, IsNSString ssid) => neHotspotConfigurationManager -> ssid -> IO ()
removeConfigurationForSSID neHotspotConfigurationManager  ssid =
  withObjCPtr ssid $ \raw_ssid ->
      sendMsg neHotspotConfigurationManager (mkSelector "removeConfigurationForSSID:") retVoid [argPtr (castPtr raw_ssid :: Ptr ())]

-- | removeConfigurationForNetworkName:
--
-- This function removes Wi-Fi configuration.
--
-- @domainName@ — HS2.0 domainName for which the configuration is to be deleted.
--
-- ObjC selector: @- removeConfigurationForHS20DomainName:@
removeConfigurationForHS20DomainName :: (IsNEHotspotConfigurationManager neHotspotConfigurationManager, IsNSString domainName) => neHotspotConfigurationManager -> domainName -> IO ()
removeConfigurationForHS20DomainName neHotspotConfigurationManager  domainName =
  withObjCPtr domainName $ \raw_domainName ->
      sendMsg neHotspotConfigurationManager (mkSelector "removeConfigurationForHS20DomainName:") retVoid [argPtr (castPtr raw_domainName :: Ptr ())]

-- | joinAccessoryHotspot:
--
-- This function performs a one-time join of a Wi-Fi network configuration defined by an ASAccessory.   This function implicitly sets joinOnce to YES. The network must support WPA/WPA2/WPA3 Personal security type.
--
-- @accessory@ — Object of type ASAccessory class.   This parameter is required to specify the Accessory Wi-Fi network.
--
-- @passphrase@ — The required passphrase credential.   The passphrase with a length between 8 and 63 characters to join WPA/WPA2/WPA3 Personal networks.
--
-- @completionHandler@ — A block that will be called when join operation is completed.   Pass nil if application does not intend to receive the result.   The NSError passed to this block will be nil if the hotspot is successfully joined, non-nil otherwise.   If the configuration is found to be invalid or some other error is encountered then the completionHandler   block is executed with with an instance of NSError containing an appropriate error code.
--
-- ObjC selector: @- joinAccessoryHotspot:passphrase:completionHandler:@
joinAccessoryHotspot_passphrase_completionHandler :: (IsNEHotspotConfigurationManager neHotspotConfigurationManager, IsASAccessory accessory, IsNSString passphrase) => neHotspotConfigurationManager -> accessory -> passphrase -> Ptr () -> IO ()
joinAccessoryHotspot_passphrase_completionHandler neHotspotConfigurationManager  accessory passphrase completionHandler =
  withObjCPtr accessory $ \raw_accessory ->
    withObjCPtr passphrase $ \raw_passphrase ->
        sendMsg neHotspotConfigurationManager (mkSelector "joinAccessoryHotspot:passphrase:completionHandler:") retVoid [argPtr (castPtr raw_accessory :: Ptr ()), argPtr (castPtr raw_passphrase :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | joinAccessoryHotspotWithoutSecurity:
--
-- This function performs a one-time join of an open Wi-Fi network configuration defined by an ASAccessory.   This function implicitly sets joinOnce to YES.
--
-- @accessory@ — Object of type ASAccessory class.   This parameter is required to specify the Accessory Wi-Fi network.
--
-- @completionHandler@ — A block that will be called when join operation is completed.   Pass nil if application does not intend to receive the result.   The NSError passed to this block will be nil if the hotspot is successfully joined, non-nil otherwise.   If the configuration is found to be invalid or some other error is encountered then the completionHandler   block is executed with with an instance of NSError containing an appropriate error code.
--
-- ObjC selector: @- joinAccessoryHotspotWithoutSecurity:completionHandler:@
joinAccessoryHotspotWithoutSecurity_completionHandler :: (IsNEHotspotConfigurationManager neHotspotConfigurationManager, IsASAccessory accessory) => neHotspotConfigurationManager -> accessory -> Ptr () -> IO ()
joinAccessoryHotspotWithoutSecurity_completionHandler neHotspotConfigurationManager  accessory completionHandler =
  withObjCPtr accessory $ \raw_accessory ->
      sendMsg neHotspotConfigurationManager (mkSelector "joinAccessoryHotspotWithoutSecurity:completionHandler:") retVoid [argPtr (castPtr raw_accessory :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ sharedManager@
sharedManager :: IO (Id NEHotspotConfigurationManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEHotspotConfigurationManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @applyConfiguration:completionHandler:@
applyConfiguration_completionHandlerSelector :: Selector
applyConfiguration_completionHandlerSelector = mkSelector "applyConfiguration:completionHandler:"

-- | @Selector@ for @removeConfigurationForSSID:@
removeConfigurationForSSIDSelector :: Selector
removeConfigurationForSSIDSelector = mkSelector "removeConfigurationForSSID:"

-- | @Selector@ for @removeConfigurationForHS20DomainName:@
removeConfigurationForHS20DomainNameSelector :: Selector
removeConfigurationForHS20DomainNameSelector = mkSelector "removeConfigurationForHS20DomainName:"

-- | @Selector@ for @joinAccessoryHotspot:passphrase:completionHandler:@
joinAccessoryHotspot_passphrase_completionHandlerSelector :: Selector
joinAccessoryHotspot_passphrase_completionHandlerSelector = mkSelector "joinAccessoryHotspot:passphrase:completionHandler:"

-- | @Selector@ for @joinAccessoryHotspotWithoutSecurity:completionHandler:@
joinAccessoryHotspotWithoutSecurity_completionHandlerSelector :: Selector
joinAccessoryHotspotWithoutSecurity_completionHandlerSelector = mkSelector "joinAccessoryHotspotWithoutSecurity:completionHandler:"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

