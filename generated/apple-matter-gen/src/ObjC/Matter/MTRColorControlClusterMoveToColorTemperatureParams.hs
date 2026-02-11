{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterMoveToColorTemperatureParams
  ( MTRColorControlClusterMoveToColorTemperatureParams
  , IsMTRColorControlClusterMoveToColorTemperatureParams(..)
  , colorTemperatureMireds
  , setColorTemperatureMireds
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
  , colorTemperature
  , setColorTemperature
  , colorTemperatureMiredsSelector
  , setColorTemperatureMiredsSelector
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
  , colorTemperatureSelector
  , setColorTemperatureSelector


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

-- | @- colorTemperatureMireds@
colorTemperatureMireds :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMireds mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "colorTemperatureMireds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperatureMireds:@
setColorTemperatureMireds :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setColorTemperatureMireds mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setColorTemperatureMireds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorTemperature@
colorTemperature :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
colorTemperature mtrColorControlClusterMoveToColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "colorTemperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperature:@
setColorTemperature :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setColorTemperature mtrColorControlClusterMoveToColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorTemperatureParams (mkSelector "setColorTemperature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorTemperatureMireds@
colorTemperatureMiredsSelector :: Selector
colorTemperatureMiredsSelector = mkSelector "colorTemperatureMireds"

-- | @Selector@ for @setColorTemperatureMireds:@
setColorTemperatureMiredsSelector :: Selector
setColorTemperatureMiredsSelector = mkSelector "setColorTemperatureMireds:"

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

-- | @Selector@ for @colorTemperature@
colorTemperatureSelector :: Selector
colorTemperatureSelector = mkSelector "colorTemperature"

-- | @Selector@ for @setColorTemperature:@
setColorTemperatureSelector :: Selector
setColorTemperatureSelector = mkSelector "setColorTemperature:"

