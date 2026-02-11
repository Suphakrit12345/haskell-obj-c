{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentAppObserverClusterContentAppMessageParams@.
module ObjC.Matter.MTRContentAppObserverClusterContentAppMessageParams
  ( MTRContentAppObserverClusterContentAppMessageParams
  , IsMTRContentAppObserverClusterContentAppMessageParams(..)
  , data_
  , setData
  , encodingHint
  , setEncodingHint
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dataSelector
  , setDataSelector
  , encodingHintSelector
  , setEncodingHintSelector
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

-- | @- data@
data_ :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSString)
data_ mtrContentAppObserverClusterContentAppMessageParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setData mtrContentAppObserverClusterContentAppMessageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encodingHint@
encodingHint :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSString)
encodingHint mtrContentAppObserverClusterContentAppMessageParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "encodingHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncodingHint:@
setEncodingHint :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setEncodingHint mtrContentAppObserverClusterContentAppMessageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "setEncodingHint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentAppObserverClusterContentAppMessageParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentAppObserverClusterContentAppMessageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentAppObserverClusterContentAppMessageParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentAppObserverClusterContentAppMessageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @encodingHint@
encodingHintSelector :: Selector
encodingHintSelector = mkSelector "encodingHint"

-- | @Selector@ for @setEncodingHint:@
setEncodingHintSelector :: Selector
setEncodingHintSelector = mkSelector "setEncodingHint:"

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

