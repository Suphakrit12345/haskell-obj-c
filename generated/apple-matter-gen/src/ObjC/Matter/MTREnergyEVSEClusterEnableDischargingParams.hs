{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnableDischargingParams@.
module ObjC.Matter.MTREnergyEVSEClusterEnableDischargingParams
  ( MTREnergyEVSEClusterEnableDischargingParams
  , IsMTREnergyEVSEClusterEnableDischargingParams(..)
  , dischargingEnabledUntil
  , setDischargingEnabledUntil
  , maximumDischargeCurrent
  , setMaximumDischargeCurrent
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dischargingEnabledUntilSelector
  , setDischargingEnabledUntilSelector
  , maximumDischargeCurrentSelector
  , setMaximumDischargeCurrentSelector
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

-- | @- dischargingEnabledUntil@
dischargingEnabledUntil :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
dischargingEnabledUntil mtrEnergyEVSEClusterEnableDischargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "dischargingEnabledUntil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDischargingEnabledUntil:@
setDischargingEnabledUntil :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setDischargingEnabledUntil mtrEnergyEVSEClusterEnableDischargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "setDischargingEnabledUntil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumDischargeCurrent@
maximumDischargeCurrent :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
maximumDischargeCurrent mtrEnergyEVSEClusterEnableDischargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "maximumDischargeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumDischargeCurrent:@
setMaximumDischargeCurrent :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setMaximumDischargeCurrent mtrEnergyEVSEClusterEnableDischargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "setMaximumDischargeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrEnergyEVSEClusterEnableDischargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrEnergyEVSEClusterEnableDischargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrEnergyEVSEClusterEnableDischargingParams  =
    sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setServerSideProcessingTimeout mtrEnergyEVSEClusterEnableDischargingParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnableDischargingParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dischargingEnabledUntil@
dischargingEnabledUntilSelector :: Selector
dischargingEnabledUntilSelector = mkSelector "dischargingEnabledUntil"

-- | @Selector@ for @setDischargingEnabledUntil:@
setDischargingEnabledUntilSelector :: Selector
setDischargingEnabledUntilSelector = mkSelector "setDischargingEnabledUntil:"

-- | @Selector@ for @maximumDischargeCurrent@
maximumDischargeCurrentSelector :: Selector
maximumDischargeCurrentSelector = mkSelector "maximumDischargeCurrent"

-- | @Selector@ for @setMaximumDischargeCurrent:@
setMaximumDischargeCurrentSelector :: Selector
setMaximumDischargeCurrentSelector = mkSelector "setMaximumDischargeCurrent:"

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

