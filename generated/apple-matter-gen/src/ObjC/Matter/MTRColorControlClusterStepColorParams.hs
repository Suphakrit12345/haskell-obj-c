{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepColorParams@.
module ObjC.Matter.MTRColorControlClusterStepColorParams
  ( MTRColorControlClusterStepColorParams
  , IsMTRColorControlClusterStepColorParams(..)
  , stepX
  , setStepX
  , stepY
  , setStepY
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
  , stepXSelector
  , setStepXSelector
  , stepYSelector
  , setStepYSelector
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

-- | @- stepX@
stepX :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
stepX mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "stepX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepX:@
setStepX :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setStepX mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setStepX:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stepY@
stepY :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
stepY mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "stepY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStepY:@
setStepY :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setStepY mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setStepY:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepColorParams  =
    sendMsg mtrColorControlClusterStepColorParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterStepColorParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepX@
stepXSelector :: Selector
stepXSelector = mkSelector "stepX"

-- | @Selector@ for @setStepX:@
setStepXSelector :: Selector
setStepXSelector = mkSelector "setStepX:"

-- | @Selector@ for @stepY@
stepYSelector :: Selector
stepYSelector = mkSelector "stepY"

-- | @Selector@ for @setStepY:@
setStepYSelector :: Selector
setStepYSelector = mkSelector "setStepY:"

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

