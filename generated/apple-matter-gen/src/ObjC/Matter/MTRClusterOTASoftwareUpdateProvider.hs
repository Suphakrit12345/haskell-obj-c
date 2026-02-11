{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterOtaSoftwareUpdateProvider@.
module ObjC.Matter.MTRClusterOtaSoftwareUpdateProvider
  ( MTRClusterOtaSoftwareUpdateProvider
  , IsMTRClusterOtaSoftwareUpdateProvider(..)
  , initWithDevice_endpoint_queue
  , queryImageWithParams_expectedValues_expectedValueInterval_completionHandler
  , applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpoint_queueSelector
  , queryImageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandlerSelector


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

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOtaSoftwareUpdateProvider mtrClusterOtaSoftwareUpdateProvider, IsMTRDevice device, IsNSObject queue) => mtrClusterOtaSoftwareUpdateProvider -> device -> CUShort -> queue -> IO (Id MTRClusterOtaSoftwareUpdateProvider)
initWithDevice_endpoint_queue mtrClusterOtaSoftwareUpdateProvider  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOtaSoftwareUpdateProvider (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- queryImageWithParams:expectedValues:expectedValueInterval:completionHandler:@
queryImageWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOtaSoftwareUpdateProvider mtrClusterOtaSoftwareUpdateProvider, IsMTROtaSoftwareUpdateProviderClusterQueryImageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOtaSoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
queryImageWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOtaSoftwareUpdateProvider  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOtaSoftwareUpdateProvider (mkSelector "queryImageWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOtaSoftwareUpdateProvider mtrClusterOtaSoftwareUpdateProvider, IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOtaSoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOtaSoftwareUpdateProvider  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOtaSoftwareUpdateProvider (mkSelector "applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completionHandler:@
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOtaSoftwareUpdateProvider mtrClusterOtaSoftwareUpdateProvider, IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOtaSoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOtaSoftwareUpdateProvider  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOtaSoftwareUpdateProvider (mkSelector "notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @queryImageWithParams:expectedValues:expectedValueInterval:completionHandler:@
queryImageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
queryImageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "queryImageWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completionHandler:@
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completionHandler:"

