{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterOtaSoftwareUpdateRequestor@.
module ObjC.Matter.MTRClusterOtaSoftwareUpdateRequestor
  ( MTRClusterOtaSoftwareUpdateRequestor
  , IsMTRClusterOtaSoftwareUpdateRequestor(..)
  , initWithDevice_endpoint_queue
  , announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandler
  , readAttributeDefaultOtaProvidersWithParams
  , writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval
  , writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_params
  , initWithDevice_endpoint_queueSelector
  , announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , readAttributeDefaultOtaProvidersWithParamsSelector
  , writeAttributeDefaultOtaProvidersWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_paramsSelector


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
initWithDevice_endpoint_queue :: (IsMTRClusterOtaSoftwareUpdateRequestor mtrClusterOtaSoftwareUpdateRequestor, IsMTRDevice device, IsNSObject queue) => mtrClusterOtaSoftwareUpdateRequestor -> device -> CUShort -> queue -> IO (Id MTRClusterOtaSoftwareUpdateRequestor)
initWithDevice_endpoint_queue mtrClusterOtaSoftwareUpdateRequestor  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOtaSoftwareUpdateRequestor (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- announceOtaProviderWithParams:expectedValues:expectedValueInterval:completionHandler:@
announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOtaSoftwareUpdateRequestor mtrClusterOtaSoftwareUpdateRequestor, IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOtaSoftwareUpdateRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOtaSoftwareUpdateRequestor  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOtaSoftwareUpdateRequestor (mkSelector "announceOtaProviderWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDefaultOtaProvidersWithParams:@
readAttributeDefaultOtaProvidersWithParams :: (IsMTRClusterOtaSoftwareUpdateRequestor mtrClusterOtaSoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOtaSoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeDefaultOtaProvidersWithParams mtrClusterOtaSoftwareUpdateRequestor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeDefaultOtaProvidersWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:@
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval :: (IsMTRClusterOtaSoftwareUpdateRequestor mtrClusterOtaSoftwareUpdateRequestor, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOtaSoftwareUpdateRequestor -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval mtrClusterOtaSoftwareUpdateRequestor  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOtaSoftwareUpdateRequestor (mkSelector "writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:params:@
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_params :: (IsMTRClusterOtaSoftwareUpdateRequestor mtrClusterOtaSoftwareUpdateRequestor, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOtaSoftwareUpdateRequestor -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_params mtrClusterOtaSoftwareUpdateRequestor  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterOtaSoftwareUpdateRequestor (mkSelector "writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @announceOtaProviderWithParams:expectedValues:expectedValueInterval:completionHandler:@
announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
announceOtaProviderWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "announceOtaProviderWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @readAttributeDefaultOtaProvidersWithParams:@
readAttributeDefaultOtaProvidersWithParamsSelector :: Selector
readAttributeDefaultOtaProvidersWithParamsSelector = mkSelector "readAttributeDefaultOtaProvidersWithParams:"

-- | @Selector@ for @writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:@
writeAttributeDefaultOtaProvidersWithValue_expectedValueIntervalSelector :: Selector
writeAttributeDefaultOtaProvidersWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:params:@
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeDefaultOtaProvidersWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOtaProvidersWithValue:expectedValueInterval:params:"

