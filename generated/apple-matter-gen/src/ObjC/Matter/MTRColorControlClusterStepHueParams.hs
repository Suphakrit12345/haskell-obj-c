{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepHueParams@.
module ObjC.Matter.MTRColorControlClusterStepHueParams
  ( MTRColorControlClusterStepHueParams
  , IsMTRColorControlClusterStepHueParams(..)
  , stepMode
  , setStepMode
  , stepSize
  , setStepSize
  , transitionTime
  , setTransitionTime
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
stepMode :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "stepMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setStepMode mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setStepMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "stepSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setStepSize mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setStepSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepHueParams  =
    sendMsg mtrColorControlClusterStepHueParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepHueParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

