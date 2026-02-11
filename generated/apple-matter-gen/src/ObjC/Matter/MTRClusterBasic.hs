{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterBasic@.
module ObjC.Matter.MTRClusterBasic
  ( MTRClusterBasic
  , IsMTRClusterBasic(..)
  , initWithDevice_endpoint_queue
  , mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler
  , mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpoint_queueSelector
  , mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector


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
initWithDevice_endpoint_queue :: (IsMTRClusterBasic mtrClusterBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterBasic -> device -> CUShort -> queue -> IO (Id MTRClusterBasic)
initWithDevice_endpoint_queue mtrClusterBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBasic mtrClusterBasic, IsMTRBasicClusterMfgSpecificPingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBasic -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBasic  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBasic (mkSelector "mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBasic mtrClusterBasic, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBasic -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler mtrClusterBasic  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBasic (mkSelector "mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:"

