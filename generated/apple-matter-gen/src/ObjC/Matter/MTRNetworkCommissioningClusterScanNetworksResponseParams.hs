{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterScanNetworksResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterScanNetworksResponseParams
  ( MTRNetworkCommissioningClusterScanNetworksResponseParams
  , IsMTRNetworkCommissioningClusterScanNetworksResponseParams(..)
  , initWithResponseValue_error
  , networkingStatus
  , setNetworkingStatus
  , debugText
  , setDebugText
  , wiFiScanResults
  , setWiFiScanResults
  , threadScanResults
  , setThreadScanResults
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , networkingStatusSelector
  , setNetworkingStatusSelector
  , debugTextSelector
  , setDebugTextSelector
  , wiFiScanResultsSelector
  , setWiFiScanResultsSelector
  , threadScanResultsSelector
  , setThreadScanResultsSelector
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

-- | Initialize an MTRNetworkCommissioningClusterScanNetworksResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterScanNetworksResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterScanNetworksResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- networkingStatus@
networkingStatus :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSNumber)
networkingStatus mtrNetworkCommissioningClusterScanNetworksResponseParams  =
    sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "networkingStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkingStatus:@
setNetworkingStatus :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setNetworkingStatus mtrNetworkCommissioningClusterScanNetworksResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "setNetworkingStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- debugText@
debugText :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSString)
debugText mtrNetworkCommissioningClusterScanNetworksResponseParams  =
    sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "debugText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDebugText:@
setDebugText :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSString value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setDebugText mtrNetworkCommissioningClusterScanNetworksResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "setDebugText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wiFiScanResults@
wiFiScanResults :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSArray)
wiFiScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams  =
    sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "wiFiScanResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWiFiScanResults:@
setWiFiScanResults :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSArray value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setWiFiScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "setWiFiScanResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- threadScanResults@
threadScanResults :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSArray)
threadScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams  =
    sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "threadScanResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThreadScanResults:@
setThreadScanResults :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSArray value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setThreadScanResults mtrNetworkCommissioningClusterScanNetworksResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "setThreadScanResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams => mtrNetworkCommissioningClusterScanNetworksResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksResponseParams  =
    sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterScanNetworksResponseParams mtrNetworkCommissioningClusterScanNetworksResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterScanNetworksResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @networkingStatus@
networkingStatusSelector :: Selector
networkingStatusSelector = mkSelector "networkingStatus"

-- | @Selector@ for @setNetworkingStatus:@
setNetworkingStatusSelector :: Selector
setNetworkingStatusSelector = mkSelector "setNetworkingStatus:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @wiFiScanResults@
wiFiScanResultsSelector :: Selector
wiFiScanResultsSelector = mkSelector "wiFiScanResults"

-- | @Selector@ for @setWiFiScanResults:@
setWiFiScanResultsSelector :: Selector
setWiFiScanResultsSelector = mkSelector "setWiFiScanResults:"

-- | @Selector@ for @threadScanResults@
threadScanResultsSelector :: Selector
threadScanResultsSelector = mkSelector "threadScanResults"

-- | @Selector@ for @setThreadScanResults:@
setThreadScanResultsSelector :: Selector
setThreadScanResultsSelector = mkSelector "setThreadScanResults:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

