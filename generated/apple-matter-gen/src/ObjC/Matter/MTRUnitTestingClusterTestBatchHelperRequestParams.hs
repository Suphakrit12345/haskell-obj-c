{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestBatchHelperRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestBatchHelperRequestParams
  ( MTRUnitTestingClusterTestBatchHelperRequestParams
  , IsMTRUnitTestingClusterTestBatchHelperRequestParams(..)
  , sleepBeforeResponseTimeMs
  , setSleepBeforeResponseTimeMs
  , sizeOfResponseBuffer
  , setSizeOfResponseBuffer
  , fillCharacter
  , setFillCharacter
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , sleepBeforeResponseTimeMsSelector
  , setSleepBeforeResponseTimeMsSelector
  , sizeOfResponseBufferSelector
  , setSizeOfResponseBufferSelector
  , fillCharacterSelector
  , setFillCharacterSelector
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

-- | @- sleepBeforeResponseTimeMs@
sleepBeforeResponseTimeMs :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
sleepBeforeResponseTimeMs mtrUnitTestingClusterTestBatchHelperRequestParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "sleepBeforeResponseTimeMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSleepBeforeResponseTimeMs:@
setSleepBeforeResponseTimeMs :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setSleepBeforeResponseTimeMs mtrUnitTestingClusterTestBatchHelperRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "setSleepBeforeResponseTimeMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sizeOfResponseBuffer@
sizeOfResponseBuffer :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
sizeOfResponseBuffer mtrUnitTestingClusterTestBatchHelperRequestParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "sizeOfResponseBuffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSizeOfResponseBuffer:@
setSizeOfResponseBuffer :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setSizeOfResponseBuffer mtrUnitTestingClusterTestBatchHelperRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "setSizeOfResponseBuffer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fillCharacter@
fillCharacter :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
fillCharacter mtrUnitTestingClusterTestBatchHelperRequestParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "fillCharacter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFillCharacter:@
setFillCharacter :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setFillCharacter mtrUnitTestingClusterTestBatchHelperRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "setFillCharacter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestBatchHelperRequestParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestBatchHelperRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestBatchHelperRequestParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestBatchHelperRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepBeforeResponseTimeMs@
sleepBeforeResponseTimeMsSelector :: Selector
sleepBeforeResponseTimeMsSelector = mkSelector "sleepBeforeResponseTimeMs"

-- | @Selector@ for @setSleepBeforeResponseTimeMs:@
setSleepBeforeResponseTimeMsSelector :: Selector
setSleepBeforeResponseTimeMsSelector = mkSelector "setSleepBeforeResponseTimeMs:"

-- | @Selector@ for @sizeOfResponseBuffer@
sizeOfResponseBufferSelector :: Selector
sizeOfResponseBufferSelector = mkSelector "sizeOfResponseBuffer"

-- | @Selector@ for @setSizeOfResponseBuffer:@
setSizeOfResponseBufferSelector :: Selector
setSizeOfResponseBufferSelector = mkSelector "setSizeOfResponseBuffer:"

-- | @Selector@ for @fillCharacter@
fillCharacterSelector :: Selector
fillCharacterSelector = mkSelector "fillCharacter"

-- | @Selector@ for @setFillCharacter:@
setFillCharacterSelector :: Selector
setFillCharacterSelector = mkSelector "setFillCharacter:"

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

