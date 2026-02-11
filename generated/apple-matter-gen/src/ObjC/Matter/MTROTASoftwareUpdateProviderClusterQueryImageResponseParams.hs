{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateProviderClusterQueryImageResponseParams@.
module ObjC.Matter.MTROtaSoftwareUpdateProviderClusterQueryImageResponseParams
  ( MTROtaSoftwareUpdateProviderClusterQueryImageResponseParams
  , IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams(..)
  , status
  , setStatus
  , delayedActionTime
  , setDelayedActionTime
  , imageURI
  , setImageURI
  , softwareVersion
  , setSoftwareVersion
  , softwareVersionString
  , setSoftwareVersionString
  , updateToken
  , setUpdateToken
  , userConsentNeeded
  , setUserConsentNeeded
  , metadataForRequestor
  , setMetadataForRequestor
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , statusSelector
  , setStatusSelector
  , delayedActionTimeSelector
  , setDelayedActionTimeSelector
  , imageURISelector
  , setImageURISelector
  , softwareVersionSelector
  , setSoftwareVersionSelector
  , softwareVersionStringSelector
  , setSoftwareVersionStringSelector
  , updateTokenSelector
  , setUpdateTokenSelector
  , userConsentNeededSelector
  , setUserConsentNeededSelector
  , metadataForRequestorSelector
  , setMetadataForRequestorSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector


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

-- | @- status@
status :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
status mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setStatus mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delayedActionTime@
delayedActionTime :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
delayedActionTime mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "delayedActionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDelayedActionTime:@
setDelayedActionTime :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setDelayedActionTime mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setDelayedActionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageURI@
imageURI :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSString)
imageURI mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "imageURI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageURI:@
setImageURI :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSString value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setImageURI mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setImageURI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- softwareVersion@
softwareVersion :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
softwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setSoftwareVersion mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- softwareVersionString@
softwareVersionString :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSString)
softwareVersionString mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "softwareVersionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersionString:@
setSoftwareVersionString :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSString value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setSoftwareVersionString mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setSoftwareVersionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- updateToken@
updateToken :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSData)
updateToken mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "updateToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSData value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setUpdateToken mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setUpdateToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userConsentNeeded@
userConsentNeeded :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
userConsentNeeded mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "userConsentNeeded") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserConsentNeeded:@
setUserConsentNeeded :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setUserConsentNeeded mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setUserConsentNeeded:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataForRequestor@
metadataForRequestor :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSData)
metadataForRequestor mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "metadataForRequestor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataForRequestor:@
setMetadataForRequestor :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSData value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setMetadataForRequestor mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setMetadataForRequestor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateProviderClusterQueryImageResponseParams mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterQueryImageResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @delayedActionTime@
delayedActionTimeSelector :: Selector
delayedActionTimeSelector = mkSelector "delayedActionTime"

-- | @Selector@ for @setDelayedActionTime:@
setDelayedActionTimeSelector :: Selector
setDelayedActionTimeSelector = mkSelector "setDelayedActionTime:"

-- | @Selector@ for @imageURI@
imageURISelector :: Selector
imageURISelector = mkSelector "imageURI"

-- | @Selector@ for @setImageURI:@
setImageURISelector :: Selector
setImageURISelector = mkSelector "setImageURI:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @softwareVersionString@
softwareVersionStringSelector :: Selector
softwareVersionStringSelector = mkSelector "softwareVersionString"

-- | @Selector@ for @setSoftwareVersionString:@
setSoftwareVersionStringSelector :: Selector
setSoftwareVersionStringSelector = mkSelector "setSoftwareVersionString:"

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @userConsentNeeded@
userConsentNeededSelector :: Selector
userConsentNeededSelector = mkSelector "userConsentNeeded"

-- | @Selector@ for @setUserConsentNeeded:@
setUserConsentNeededSelector :: Selector
setUserConsentNeededSelector = mkSelector "setUserConsentNeeded:"

-- | @Selector@ for @metadataForRequestor@
metadataForRequestorSelector :: Selector
metadataForRequestorSelector = mkSelector "metadataForRequestor"

-- | @Selector@ for @setMetadataForRequestor:@
setMetadataForRequestorSelector :: Selector
setMetadataForRequestorSelector = mkSelector "setMetadataForRequestor:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

