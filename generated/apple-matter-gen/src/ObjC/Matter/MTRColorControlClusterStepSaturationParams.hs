{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepSaturationParams@.
module ObjC.Matter.MTRColorControlClusterStepSaturationParams
  ( MTRColorControlClusterStepSaturationParams
  , IsMTRColorControlClusterStepSaturationParams(..)
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
stepMode :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "stepMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setStepMode mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setStepMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "stepSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setStepSize mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setStepSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepSaturationParams  =
    sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepSaturationParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

