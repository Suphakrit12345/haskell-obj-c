{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterStepColorTemperatureParams
  ( MTRColorControlClusterStepColorTemperatureParams
  , IsMTRColorControlClusterStepColorTemperatureParams(..)
  , stepMode
  , setStepMode
  , stepSize
  , setStepSize
  , transitionTime
  , setTransitionTime
  , colorTemperatureMinimumMireds
  , setColorTemperatureMinimumMireds
  , colorTemperatureMaximumMireds
  , setColorTemperatureMaximumMireds
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , stepModeSelector
  , setStepModeSelector
  , stepSizeSelector
  , setStepSizeSelector
  , transitionTimeSelector
  , setTransitionTimeSelector
  , colorTemperatureMinimumMiredsSelector
  , setColorTemperatureMinimumMiredsSelector
  , colorTemperatureMaximumMiredsSelector
  , setColorTemperatureMaximumMiredsSelector
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

-- | @- stepMode@
stepMode :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "stepMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setStepMode mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setStepMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "stepSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setStepSize mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setStepSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorTemperatureMinimumMireds@
colorTemperatureMinimumMireds :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMinimumMireds mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "colorTemperatureMinimumMireds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMireds :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setColorTemperatureMinimumMireds mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setColorTemperatureMinimumMireds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorTemperatureMaximumMireds@
colorTemperatureMaximumMireds :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMaximumMireds mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "colorTemperatureMaximumMireds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMireds :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setColorTemperatureMaximumMireds mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setColorTemperatureMaximumMireds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepColorTemperatureParams  =
    sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorTemperatureParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepMode@
stepModeSelector :: Selector
stepModeSelector = mkSelector "stepMode"

-- | @Selector@ for @setStepMode:@
setStepModeSelector :: Selector
setStepModeSelector = mkSelector "setStepMode:"

-- | @Selector@ for @stepSize@
stepSizeSelector :: Selector
stepSizeSelector = mkSelector "stepSize"

-- | @Selector@ for @setStepSize:@
setStepSizeSelector :: Selector
setStepSizeSelector = mkSelector "setStepSize:"

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @colorTemperatureMinimumMireds@
colorTemperatureMinimumMiredsSelector :: Selector
colorTemperatureMinimumMiredsSelector = mkSelector "colorTemperatureMinimumMireds"

-- | @Selector@ for @setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMiredsSelector :: Selector
setColorTemperatureMinimumMiredsSelector = mkSelector "setColorTemperatureMinimumMireds:"

-- | @Selector@ for @colorTemperatureMaximumMireds@
colorTemperatureMaximumMiredsSelector :: Selector
colorTemperatureMaximumMiredsSelector = mkSelector "colorTemperatureMaximumMireds"

-- | @Selector@ for @setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMiredsSelector :: Selector
setColorTemperatureMaximumMiredsSelector = mkSelector "setColorTemperatureMaximumMireds:"

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

