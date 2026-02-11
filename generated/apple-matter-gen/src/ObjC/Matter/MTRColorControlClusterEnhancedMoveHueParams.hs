{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterEnhancedMoveHueParams@.
module ObjC.Matter.MTRColorControlClusterEnhancedMoveHueParams
  ( MTRColorControlClusterEnhancedMoveHueParams
  , IsMTRColorControlClusterEnhancedMoveHueParams(..)
  , moveMode
  , setMoveMode
  , rate
  , setRate
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
moveMode :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
moveMode mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "moveMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMoveMode:@
setMoveMode :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setMoveMode mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setMoveMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rate@
rate :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
rate mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "rate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRate:@
setRate :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setRate mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams => mtrColorControlClusterEnhancedMoveHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterEnhancedMoveHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterEnhancedMoveHueParams mtrColorControlClusterEnhancedMoveHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterEnhancedMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveHueParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

