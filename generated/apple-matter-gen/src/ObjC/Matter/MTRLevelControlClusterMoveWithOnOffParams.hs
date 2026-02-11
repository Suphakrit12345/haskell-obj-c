{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterMoveWithOnOffParams@.
module ObjC.Matter.MTRLevelControlClusterMoveWithOnOffParams
  ( MTRLevelControlClusterMoveWithOnOffParams
  , IsMTRLevelControlClusterMoveWithOnOffParams(..)
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
moveMode :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
moveMode mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "moveMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMoveMode:@
setMoveMode :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setMoveMode mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setMoveMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rate@
rate :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
rate mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "rate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRate:@
setRate :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setRate mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams => mtrLevelControlClusterMoveWithOnOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterMoveWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterMoveWithOnOffParams mtrLevelControlClusterMoveWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveWithOnOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterMoveWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveWithOnOffParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

