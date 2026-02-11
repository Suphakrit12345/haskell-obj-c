{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveHueParams@.
module ObjC.Matter.MTRColorControlClusterMoveHueParams
  ( MTRColorControlClusterMoveHueParams
  , IsMTRColorControlClusterMoveHueParams(..)
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
moveMode :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
moveMode mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "moveMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMoveMode:@
setMoveMode :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setMoveMode mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setMoveMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rate@
rate :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
rate mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "rate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRate:@
setRate :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setRate mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams => mtrColorControlClusterMoveHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveHueParams  =
    sendMsg mtrColorControlClusterMoveHueParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveHueParams mtrColorControlClusterMoveHueParams, IsNSNumber value) => mtrColorControlClusterMoveHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveHueParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

