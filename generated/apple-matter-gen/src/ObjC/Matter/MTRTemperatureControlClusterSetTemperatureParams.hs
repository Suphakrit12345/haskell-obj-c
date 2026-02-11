{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTemperatureControlClusterSetTemperatureParams@.
module ObjC.Matter.MTRTemperatureControlClusterSetTemperatureParams
  ( MTRTemperatureControlClusterSetTemperatureParams
  , IsMTRTemperatureControlClusterSetTemperatureParams(..)
  , targetTemperature
  , setTargetTemperature
  , targetTemperatureLevel
  , setTargetTemperatureLevel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , targetTemperatureSelector
  , setTargetTemperatureSelector
  , targetTemperatureLevelSelector
  , setTargetTemperatureLevelSelector
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

-- | @- targetTemperature@
targetTemperature :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
targetTemperature mtrTemperatureControlClusterSetTemperatureParams  =
    sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "targetTemperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetTemperature:@
setTargetTemperature :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTargetTemperature mtrTemperatureControlClusterSetTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "setTargetTemperature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetTemperatureLevel@
targetTemperatureLevel :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
targetTemperatureLevel mtrTemperatureControlClusterSetTemperatureParams  =
    sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "targetTemperatureLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetTemperatureLevel:@
setTargetTemperatureLevel :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTargetTemperatureLevel mtrTemperatureControlClusterSetTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "setTargetTemperatureLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTemperatureControlClusterSetTemperatureParams  =
    sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTemperatureControlClusterSetTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTemperatureControlClusterSetTemperatureParams  =
    sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrTemperatureControlClusterSetTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTemperatureControlClusterSetTemperatureParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetTemperature@
targetTemperatureSelector :: Selector
targetTemperatureSelector = mkSelector "targetTemperature"

-- | @Selector@ for @setTargetTemperature:@
setTargetTemperatureSelector :: Selector
setTargetTemperatureSelector = mkSelector "setTargetTemperature:"

-- | @Selector@ for @targetTemperatureLevel@
targetTemperatureLevelSelector :: Selector
targetTemperatureLevelSelector = mkSelector "targetTemperatureLevel"

-- | @Selector@ for @setTargetTemperatureLevel:@
setTargetTemperatureLevelSelector :: Selector
setTargetTemperatureLevelSelector = mkSelector "setTargetTemperatureLevel:"

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

