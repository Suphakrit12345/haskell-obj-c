{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDiagnosticLogsClusterRetrieveLogsResponseParams@.
module ObjC.Matter.MTRDiagnosticLogsClusterRetrieveLogsResponseParams
  ( MTRDiagnosticLogsClusterRetrieveLogsResponseParams
  , IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , logContent
  , setLogContent
  , utcTimeStamp
  , setUtcTimeStamp
  , timeSinceBoot
  , setTimeSinceBoot
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , content
  , setContent
  , timeStamp
  , setTimeStamp
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , logContentSelector
  , setLogContentSelector
  , utcTimeStampSelector
  , setUtcTimeStampSelector
  , timeSinceBootSelector
  , setTimeSinceBootSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , contentSelector
  , setContentSelector
  , timeStampSelector
  , setTimeStampSelector


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

-- | Initialize an MTRDiagnosticLogsClusterRetrieveLogsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> responseValue -> error_ -> IO (Id MTRDiagnosticLogsClusterRetrieveLogsResponseParams)
initWithResponseValue_error mtrDiagnosticLogsClusterRetrieveLogsResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSNumber)
status mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setStatus mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- logContent@
logContent :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSData)
logContent mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "logContent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLogContent:@
setLogContent :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSData value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setLogContent mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setLogContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- utcTimeStamp@
utcTimeStamp :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSNumber)
utcTimeStamp mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "utcTimeStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUtcTimeStamp:@
setUtcTimeStamp :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setUtcTimeStamp mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setUtcTimeStamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeSinceBoot@
timeSinceBoot :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSNumber)
timeSinceBoot mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "timeSinceBoot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeSinceBoot:@
setTimeSinceBoot :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setTimeSinceBoot mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setTimeSinceBoot:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- content@
content :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSData)
content mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContent:@
setContent :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSData value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setContent mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeStamp@
timeStamp :: IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> IO (Id NSNumber)
timeStamp mtrDiagnosticLogsClusterRetrieveLogsResponseParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "timeStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeStamp:@
setTimeStamp :: (IsMTRDiagnosticLogsClusterRetrieveLogsResponseParams mtrDiagnosticLogsClusterRetrieveLogsResponseParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsResponseParams -> value -> IO ()
setTimeStamp mtrDiagnosticLogsClusterRetrieveLogsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsResponseParams (mkSelector "setTimeStamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @logContent@
logContentSelector :: Selector
logContentSelector = mkSelector "logContent"

-- | @Selector@ for @setLogContent:@
setLogContentSelector :: Selector
setLogContentSelector = mkSelector "setLogContent:"

-- | @Selector@ for @utcTimeStamp@
utcTimeStampSelector :: Selector
utcTimeStampSelector = mkSelector "utcTimeStamp"

-- | @Selector@ for @setUtcTimeStamp:@
setUtcTimeStampSelector :: Selector
setUtcTimeStampSelector = mkSelector "setUtcTimeStamp:"

-- | @Selector@ for @timeSinceBoot@
timeSinceBootSelector :: Selector
timeSinceBootSelector = mkSelector "timeSinceBoot"

-- | @Selector@ for @setTimeSinceBoot:@
setTimeSinceBootSelector :: Selector
setTimeSinceBootSelector = mkSelector "setTimeSinceBoot:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @timeStamp@
timeStampSelector :: Selector
timeStampSelector = mkSelector "timeStamp"

-- | @Selector@ for @setTimeStamp:@
setTimeStampSelector :: Selector
setTimeStampSelector = mkSelector "setTimeStamp:"

