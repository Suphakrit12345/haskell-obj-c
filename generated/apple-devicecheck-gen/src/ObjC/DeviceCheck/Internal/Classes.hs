{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.DeviceCheck.Internal.Classes (
    module ObjC.DeviceCheck.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- DCAppAttestService ----------

-- | A service that you use to validate the instance of your app running on a device.
--
-- Use the ``DeviceCheck/DCAppAttestService/sharedService`` instance of the ``DeviceCheck/DCAppAttestService`` class to assert the legitimacy of a particular instance of your app to your server. After ensuring service availability by reading the ``DeviceCheck/DCAppAttestService/supported`` property, you use the service to:
--
-- - Create a cryptographic key in the Secure Enclave by calling the ``DeviceCheck/DCAppAttestService/generateKeyWithCompletionHandler:`` method. - Ask Apple to certify the key by calling the ``DeviceCheck/DCAppAttestService/attestKey:clientDataHash:completionHandler:`` method. - Prepare an assertion of your app’s integrity to accompany any or all server requests using the ``DeviceCheck/DCAppAttestService/generateAssertion:clientDataHash:completionHandler:`` method.
--
-- For more information about how to support App Attest in your app, see <doc:establishing-your-app-s-integrity>. For information about the complementary procedures you implement on your server, see <doc:validating-apps-that-connect-to-your-server>.
--
-- - Note: To use the App Attest service, your app must have an app ID that you register on the [Apple Developer](https://developer.apple.com/account/) website.
-- 
-- Phantom type for @DCAppAttestService@.
data DCAppAttestService

instance IsObjCObject (Id DCAppAttestService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DCAppAttestService"

class IsNSObject a => IsDCAppAttestService a where
  toDCAppAttestService :: a -> Id DCAppAttestService

instance IsDCAppAttestService (Id DCAppAttestService) where
  toDCAppAttestService = unsafeCastId

instance IsNSObject (Id DCAppAttestService) where
  toNSObject = unsafeCastId

-- ---------- DCDevice ----------

-- | A representation of a device that provides a unique, authenticated token.
--
-- Use the shared instance of the @DCDevice@ class to generate a token that identifies a device. Call the ``DeviceCheck/DCDevice/generateTokenWithCompletionHandler:`` method to get the token, and then send it to your server:
--
-- ```swift if DCDevice.current.isSupported { // Always test for availability.     DCDevice.current.generateToken { token, error in         guard error == nil else { /* Handle the error. */ }
--
-- // Send the token to your server.     } } ```
--
-- On your server, combine the token with an authentication key that you obtain from Apple, and use the result to request access to two per-device binary digits (bits). After authenticating the device, Apple passes the current values of the bits, along with the date they were last modified, to your server. Your server applies its business logic to this information and communicates the results to your app. For more information about server-side procedures, see <doc:accessing-and-modifying-per-device-data>.
--
-- - Note: To use the @DCDevice@ class, your app must have an app ID that you register on the [Apple Developer](https://developer.apple.com/account/) website.
--
-- Apple records the bits for you, and reports the bits back to you, but you’re responsible for keeping track of what the bits mean. You’re also responsible for determining when to reset the bits for a given device; for example, when a user sells the device to someone else.
-- 
-- Phantom type for @DCDevice@.
data DCDevice

instance IsObjCObject (Id DCDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DCDevice"

class IsNSObject a => IsDCDevice a where
  toDCDevice :: a -> Id DCDevice

instance IsDCDevice (Id DCDevice) where
  toDCDevice = unsafeCastId

instance IsNSObject (Id DCDevice) where
  toNSObject = unsafeCastId
