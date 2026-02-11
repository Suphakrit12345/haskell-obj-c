{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateProviderClusterQueryImageParams@.
module ObjC.Matter.MTROtaSoftwareUpdateProviderClusterQueryImageParams
  ( MTROtaSoftwareUpdateProviderClusterQueryImageParams
  , IsMTROtaSoftwareUpdateProviderClusterQueryImageParams(..)
  , softwareVersion
  , setSoftwareVersion
  , protocolsSupported
  , setProtocolsSupported
  , hardwareVersion
  , setHardwareVersion
  , location
  , setLocation
  , requestorCanConsent
  , setRequestorCanConsent
  , metadataForProvider
  , setMetadataForProvider
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , softwareVersionSelector
  , setSoftwareVersionSelector
  , protocolsSupportedSelector
  , setProtocolsSupportedSelector
  , hardwareVersionSelector
  , setHardwareVersionSelector
  , locationSelector
  , setLocationSelector
  , requestorCanConsentSelector
  , setRequestorCanConsentSelector
  , metadataForProviderSelector
  , setMetadataForProviderSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- softwareVersion@
softwareVersion :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
softwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setSoftwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- protocolsSupported@
protocolsSupported :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSArray)
protocolsSupported mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "protocolsSupported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProtocolsSupported:@
setProtocolsSupported :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSArray value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setProtocolsSupported mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setProtocolsSupported:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hardwareVersion@
hardwareVersion :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
hardwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "hardwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHardwareVersion:@
setHardwareVersion :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setHardwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setHardwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- location@
location :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSString)
location mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSString value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setLocation mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requestorCanConsent@
requestorCanConsent :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
requestorCanConsent mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "requestorCanConsent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestorCanConsent:@
setRequestorCanConsent :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setRequestorCanConsent mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setRequestorCanConsent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataForProvider@
metadataForProvider :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSData)
metadataForProvider mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "metadataForProvider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataForProvider:@
setMetadataForProvider :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSData value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setMetadataForProvider mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setMetadataForProvider:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterQueryImageParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageParams mtrOtaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setServerSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterQueryImageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @protocolsSupported@
protocolsSupportedSelector :: Selector
protocolsSupportedSelector = mkSelector "protocolsSupported"

-- | @Selector@ for @setProtocolsSupported:@
setProtocolsSupportedSelector :: Selector
setProtocolsSupportedSelector = mkSelector "setProtocolsSupported:"

-- | @Selector@ for @hardwareVersion@
hardwareVersionSelector :: Selector
hardwareVersionSelector = mkSelector "hardwareVersion"

-- | @Selector@ for @setHardwareVersion:@
setHardwareVersionSelector :: Selector
setHardwareVersionSelector = mkSelector "setHardwareVersion:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @requestorCanConsent@
requestorCanConsentSelector :: Selector
requestorCanConsentSelector = mkSelector "requestorCanConsent"

-- | @Selector@ for @setRequestorCanConsent:@
setRequestorCanConsentSelector :: Selector
setRequestorCanConsentSelector = mkSelector "setRequestorCanConsent:"

-- | @Selector@ for @metadataForProvider@
metadataForProviderSelector :: Selector
metadataForProviderSelector = mkSelector "metadataForProvider"

-- | @Selector@ for @setMetadataForProvider:@
setMetadataForProviderSelector :: Selector
setMetadataForProviderSelector = mkSelector "setMetadataForProvider:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

