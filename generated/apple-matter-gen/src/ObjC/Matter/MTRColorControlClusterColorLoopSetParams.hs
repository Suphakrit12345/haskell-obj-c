{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterColorLoopSetParams@.
module ObjC.Matter.MTRColorControlClusterColorLoopSetParams
  ( MTRColorControlClusterColorLoopSetParams
  , IsMTRColorControlClusterColorLoopSetParams(..)
  , updateFlags
  , setUpdateFlags
  , action
  , setAction
  , direction
  , setDirection
  , time
  , setTime
  , startHue
  , setStartHue
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , updateFlagsSelector
  , setUpdateFlagsSelector
  , actionSelector
  , setActionSelector
  , directionSelector
  , setDirectionSelector
  , timeSelector
  , setTimeSelector
  , startHueSelector
  , setStartHueSelector
  , optionsMaskSelector
  , setOptionsMaskSelector
  , optionsOverrideSelector
  , setOptionsOverrideSelector
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

-- | @- updateFlags@
updateFlags :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
updateFlags mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "updateFlags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateFlags:@
setUpdateFlags :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setUpdateFlags mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setUpdateFlags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- action@
action :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
action mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "action") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAction:@
setAction :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setAction mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- direction@
direction :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
direction mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "direction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setDirection mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setDirection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- time@
time :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
time mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "time") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTime:@
setTime :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setTime mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startHue@
startHue :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
startHue mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "startHue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartHue:@
setStartHue :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setStartHue mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setStartHue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setOptionsMask mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterColorLoopSetParams  =
    sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterColorLoopSetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterColorLoopSetParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateFlags@
updateFlagsSelector :: Selector
updateFlagsSelector = mkSelector "updateFlags"

-- | @Selector@ for @setUpdateFlags:@
setUpdateFlagsSelector :: Selector
setUpdateFlagsSelector = mkSelector "setUpdateFlags:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @time@
timeSelector :: Selector
timeSelector = mkSelector "time"

-- | @Selector@ for @setTime:@
setTimeSelector :: Selector
setTimeSelector = mkSelector "setTime:"

-- | @Selector@ for @startHue@
startHueSelector :: Selector
startHueSelector = mkSelector "startHue"

-- | @Selector@ for @setStartHue:@
setStartHueSelector :: Selector
setStartHueSelector = mkSelector "setStartHue:"

-- | @Selector@ for @optionsMask@
optionsMaskSelector :: Selector
optionsMaskSelector = mkSelector "optionsMask"

-- | @Selector@ for @setOptionsMask:@
setOptionsMaskSelector :: Selector
setOptionsMaskSelector = mkSelector "setOptionsMask:"

-- | @Selector@ for @optionsOverride@
optionsOverrideSelector :: Selector
optionsOverrideSelector = mkSelector "optionsOverride"

-- | @Selector@ for @setOptionsOverride:@
setOptionsOverrideSelector :: Selector
setOptionsOverrideSelector = mkSelector "setOptionsOverride:"

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

