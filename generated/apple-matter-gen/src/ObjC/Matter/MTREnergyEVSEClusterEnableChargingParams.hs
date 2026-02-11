{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnableChargingParams@.
module ObjC.Matter.MTREnergyEVSEClusterEnableChargingParams
  ( MTREnergyEVSEClusterEnableChargingParams
  , IsMTREnergyEVSEClusterEnableChargingParams(..)
  , chargingEnabledUntil
  , setChargingEnabledUntil
  , minimumChargeCurrent
  , setMinimumChargeCurrent
  , maximumChargeCurrent
  , setMaximumChargeCurrent
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , chargingEnabledUntilSelector
  , setChargingEnabledUntilSelector
  , minimumChargeCurrentSelector
  , setMinimumChargeCurrentSelector
  , maximumChargeCurrentSelector
  , setMaximumChargeCurrentSelector
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

-- | @- chargingEnabledUntil@
chargingEnabledUntil :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
chargingEnabledUntil mtrEnergyEVSEClusterEnableChargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "chargingEnabledUntil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChargingEnabledUntil:@
setChargingEnabledUntil :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setChargingEnabledUntil mtrEnergyEVSEClusterEnableChargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "setChargingEnabledUntil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumChargeCurrent@
minimumChargeCurrent :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
minimumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "minimumChargeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinimumChargeCurrent:@
setMinimumChargeCurrent :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setMinimumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "setMinimumChargeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumChargeCurrent@
maximumChargeCurrent :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
maximumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "maximumChargeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumChargeCurrent:@
setMaximumChargeCurrent :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setMaximumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "setMaximumChargeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrEnergyEVSEClusterEnableChargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrEnergyEVSEClusterEnableChargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrEnergyEVSEClusterEnableChargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setServerSideProcessingTimeout mtrEnergyEVSEClusterEnableChargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableChargingParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chargingEnabledUntil@
chargingEnabledUntilSelector :: Selector
chargingEnabledUntilSelector = mkSelector "chargingEnabledUntil"

-- | @Selector@ for @setChargingEnabledUntil:@
setChargingEnabledUntilSelector :: Selector
setChargingEnabledUntilSelector = mkSelector "setChargingEnabledUntil:"

-- | @Selector@ for @minimumChargeCurrent@
minimumChargeCurrentSelector :: Selector
minimumChargeCurrentSelector = mkSelector "minimumChargeCurrent"

-- | @Selector@ for @setMinimumChargeCurrent:@
setMinimumChargeCurrentSelector :: Selector
setMinimumChargeCurrentSelector = mkSelector "setMinimumChargeCurrent:"

-- | @Selector@ for @maximumChargeCurrent@
maximumChargeCurrentSelector :: Selector
maximumChargeCurrentSelector = mkSelector "maximumChargeCurrent"

-- | @Selector@ for @setMaximumChargeCurrent:@
setMaximumChargeCurrentSelector :: Selector
setMaximumChargeCurrentSelector = mkSelector "setMaximumChargeCurrent:"

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

