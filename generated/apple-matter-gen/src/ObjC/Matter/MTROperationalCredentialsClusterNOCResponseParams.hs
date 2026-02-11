{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterNOCResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterNOCResponseParams
  ( MTROperationalCredentialsClusterNOCResponseParams
  , IsMTROperationalCredentialsClusterNOCResponseParams(..)
  , initWithResponseValue_error
  , statusCode
  , setStatusCode
  , fabricIndex
  , setFabricIndex
  , debugText
  , setDebugText
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , statusCodeSelector
  , setStatusCodeSelector
  , fabricIndexSelector
  , setFabricIndexSelector
  , debugTextSelector
  , setDebugTextSelector
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

-- | Initialize an MTROperationalCredentialsClusterNOCResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterNOCResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterNOCResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterNOCResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- statusCode@
statusCode :: IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams => mtrOperationalCredentialsClusterNOCResponseParams -> IO (Id NSNumber)
statusCode mtrOperationalCredentialsClusterNOCResponseParams  =
    sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterNOCResponseParams -> value -> IO ()
setStatusCode mtrOperationalCredentialsClusterNOCResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams => mtrOperationalCredentialsClusterNOCResponseParams -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterNOCResponseParams  =
    sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterNOCResponseParams -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterNOCResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- debugText@
debugText :: IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams => mtrOperationalCredentialsClusterNOCResponseParams -> IO (Id NSString)
debugText mtrOperationalCredentialsClusterNOCResponseParams  =
    sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "debugText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDebugText:@
setDebugText :: (IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams, IsNSString value) => mtrOperationalCredentialsClusterNOCResponseParams -> value -> IO ()
setDebugText mtrOperationalCredentialsClusterNOCResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "setDebugText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams => mtrOperationalCredentialsClusterNOCResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterNOCResponseParams  =
    sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterNOCResponseParams mtrOperationalCredentialsClusterNOCResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterNOCResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterNOCResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector
setStatusCodeSelector = mkSelector "setStatusCode:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

