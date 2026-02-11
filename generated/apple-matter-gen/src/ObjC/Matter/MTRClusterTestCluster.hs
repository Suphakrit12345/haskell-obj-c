{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterTestCluster@.
module ObjC.Matter.MTRClusterTestCluster
  ( MTRClusterTestCluster
  , IsMTRClusterTestCluster(..)
  , initWithDevice_endpoint_queue
  , testWithParams_expectedValues_expectedValueInterval_completionHandler
  , testWithExpectedValues_expectedValueInterval_completionHandler
  , testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandler
  , testNotHandledWithExpectedValues_expectedValueInterval_completionHandler
  , testSpecificWithParams_expectedValues_expectedValueInterval_completionHandler
  , testSpecificWithExpectedValues_expectedValueInterval_completionHandler
  , testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandler
  , testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandler
  , testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandler
  , testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandler
  , testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandler
  , testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandler
  , testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpoint_queueSelector
  , testWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testNotHandledWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testSpecificWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testSpecificWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector


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
initWithDevice_endpoint_queue :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRDevice device, IsNSObject queue) => mtrClusterTestCluster -> device -> CUShort -> queue -> IO (Id MTRClusterTestCluster)
initWithDevice_endpoint_queue mtrClusterTestCluster  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterTestCluster (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- testWithParams:expectedValues:expectedValueInterval:completionHandler:@
testWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testWithExpectedValues:expectedValueInterval:completionHandler:@
testWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNotHandledWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestNotHandledParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testNotHandledWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNotHandledWithExpectedValues:expectedValueInterval:completionHandler:@
testNotHandledWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testNotHandledWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testNotHandledWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testSpecificWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSpecificWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestSpecificParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testSpecificWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testSpecificWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testSpecificWithExpectedValues:expectedValueInterval:completionHandler:@
testSpecificWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testSpecificWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testSpecificWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testUnknownCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestUnknownCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testUnknownCommandWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testUnknownCommandWithExpectedValues:expectedValueInterval:completionHandler:@
testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testUnknownCommandWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testAddArgumentsWithParams:expectedValues:expectedValueInterval:completionHandler:@
testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestAddArgumentsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testAddArgumentsWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testSimpleArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestSimpleArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testSimpleArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testStructArrayArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestStructArrayArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testStructArrayArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestStructArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNestedStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestNestedStructArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testNestedStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testListStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestListStructArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testListStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testListInt8UArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestListInt8UArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testListInt8UArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestNestedStructListArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testListNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestListNestedStructListArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testListNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testListInt8UReverseRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestListInt8UReverseRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testListInt8UReverseRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testEnumsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestEnumsRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testEnumsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestNullableOptionalRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testNullableOptionalRequestWithExpectedValues:expectedValueInterval:completionHandler:@
testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testNullableOptionalRequestWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testComplexNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testComplexNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- simpleStructEchoRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterSimpleStructEchoRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "simpleStructEchoRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- timedInvokeRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTimedInvokeRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "timedInvokeRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- timedInvokeRequestWithExpectedValues:expectedValueInterval:completionHandler:@
timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "timedInvokeRequestWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testSimpleOptionalArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testSimpleOptionalArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testSimpleOptionalArgumentRequestWithExpectedValues:expectedValueInterval:completionHandler:@
testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterTestCluster (mkSelector "testSimpleOptionalArgumentRequestWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testEmitTestEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestEmitTestEventRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testEmitTestEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- testEmitTestFabricScopedEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTestCluster mtrClusterTestCluster, IsMTRTestClusterClusterTestEmitTestFabricScopedEventRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTestCluster -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTestCluster  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTestCluster (mkSelector "testEmitTestFabricScopedEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @testWithParams:expectedValues:expectedValueInterval:completionHandler:@
testWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testWithExpectedValues:expectedValueInterval:completionHandler:@
testWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNotHandledWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNotHandledWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNotHandledWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNotHandledWithExpectedValues:expectedValueInterval:completionHandler:@
testNotHandledWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNotHandledWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNotHandledWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testSpecificWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSpecificWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testSpecificWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testSpecificWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testSpecificWithExpectedValues:expectedValueInterval:completionHandler:@
testSpecificWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testSpecificWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testSpecificWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testUnknownCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testUnknownCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testUnknownCommandWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testUnknownCommandWithExpectedValues:expectedValueInterval:completionHandler:@
testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testUnknownCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testUnknownCommandWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testAddArgumentsWithParams:expectedValues:expectedValueInterval:completionHandler:@
testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testAddArgumentsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testAddArgumentsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testSimpleArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testSimpleArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testSimpleArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testStructArrayArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testStructArrayArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testStructArrayArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNestedStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNestedStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNestedStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testListStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testListStructArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testListStructArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testListInt8UArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testListInt8UArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testListInt8UArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testListNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testListNestedStructListArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testListNestedStructListArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testListInt8UReverseRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testListInt8UReverseRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testListInt8UReverseRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testEnumsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testEnumsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testEnumsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testNullableOptionalRequestWithExpectedValues:expectedValueInterval:completionHandler:@
testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testNullableOptionalRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testNullableOptionalRequestWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testComplexNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testComplexNullableOptionalRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testComplexNullableOptionalRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @simpleStructEchoRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
simpleStructEchoRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "simpleStructEchoRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @timedInvokeRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
timedInvokeRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "timedInvokeRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @timedInvokeRequestWithExpectedValues:expectedValueInterval:completionHandler:@
timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
timedInvokeRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "timedInvokeRequestWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testSimpleOptionalArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testSimpleOptionalArgumentRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testSimpleOptionalArgumentRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testSimpleOptionalArgumentRequestWithExpectedValues:expectedValueInterval:completionHandler:@
testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testSimpleOptionalArgumentRequestWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testSimpleOptionalArgumentRequestWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testEmitTestEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testEmitTestEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testEmitTestEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @testEmitTestFabricScopedEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testEmitTestFabricScopedEventRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testEmitTestFabricScopedEventRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

