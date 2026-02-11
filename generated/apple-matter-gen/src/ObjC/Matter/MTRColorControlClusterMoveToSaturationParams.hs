{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToSaturationParams@.
module ObjC.Matter.MTRColorControlClusterMoveToSaturationParams
  ( MTRColorControlClusterMoveToSaturationParams
  , IsMTRColorControlClusterMoveToSaturationParams(..)
  , saturation
  , setSaturation
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
  , saturationSelector
  , setSaturationSelector
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

-- | @- saturation@
saturation :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
saturation mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "saturation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSaturation:@
setSaturation :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setSaturation mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setSaturation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToSaturationParams  =
    sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToSaturationParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saturation@
saturationSelector :: Selector
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector
setSaturationSelector = mkSelector "setSaturation:"

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

