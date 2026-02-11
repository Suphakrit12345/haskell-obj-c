{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterMoveColorTemperatureParams
  ( MTRColorControlClusterMoveColorTemperatureParams
  , IsMTRColorControlClusterMoveColorTemperatureParams(..)
  , moveMode
  , setMoveMode
  , rate
  , setRate
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
  , moveModeSelector
  , setMoveModeSelector
  , rateSelector
  , setRateSelector
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

-- | @- moveMode@
moveMode :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
moveMode mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "moveMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMoveMode:@
setMoveMode :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setMoveMode mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setMoveMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rate@
rate :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
rate mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "rate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRate:@
setRate :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setRate mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorTemperatureMinimumMireds@
colorTemperatureMinimumMireds :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMinimumMireds mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "colorTemperatureMinimumMireds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMireds :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setColorTemperatureMinimumMireds mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setColorTemperatureMinimumMireds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorTemperatureMaximumMireds@
colorTemperatureMaximumMireds :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMaximumMireds mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "colorTemperatureMaximumMireds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMireds :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setColorTemperatureMaximumMireds mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setColorTemperatureMaximumMireds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveColorTemperatureParams  =
    sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveColorTemperatureParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorTemperatureParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveMode@
moveModeSelector :: Selector
moveModeSelector = mkSelector "moveMode"

-- | @Selector@ for @setMoveMode:@
setMoveModeSelector :: Selector
setMoveModeSelector = mkSelector "setMoveMode:"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

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

