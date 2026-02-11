{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterStepParams@.
module ObjC.Matter.MTRLevelControlClusterStepParams
  ( MTRLevelControlClusterStepParams
  , IsMTRLevelControlClusterStepParams(..)
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
stepMode :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
stepMode mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "stepMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepMode:@
setStepMode :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setStepMode mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setStepMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stepSize@
stepSize :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
stepSize mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "stepSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepSize:@
setStepSize :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setStepSize mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setStepSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterStepParams  =
    sendMsg mtrLevelControlClusterStepParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStepParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

