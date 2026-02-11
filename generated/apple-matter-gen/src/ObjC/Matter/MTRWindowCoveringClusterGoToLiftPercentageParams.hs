{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWindowCoveringClusterGoToLiftPercentageParams@.
module ObjC.Matter.MTRWindowCoveringClusterGoToLiftPercentageParams
  ( MTRWindowCoveringClusterGoToLiftPercentageParams
  , IsMTRWindowCoveringClusterGoToLiftPercentageParams(..)
  , liftPercent100thsValue
  , setLiftPercent100thsValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , liftPercent100thsValueSelector
  , setLiftPercent100thsValueSelector
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

-- | @- liftPercent100thsValue@
liftPercent100thsValue :: IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams => mtrWindowCoveringClusterGoToLiftPercentageParams -> IO (Id NSNumber)
liftPercent100thsValue mtrWindowCoveringClusterGoToLiftPercentageParams  =
    sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "liftPercent100thsValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLiftPercent100thsValue:@
setLiftPercent100thsValue :: (IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToLiftPercentageParams -> value -> IO ()
setLiftPercent100thsValue mtrWindowCoveringClusterGoToLiftPercentageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "setLiftPercent100thsValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams => mtrWindowCoveringClusterGoToLiftPercentageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWindowCoveringClusterGoToLiftPercentageParams  =
    sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToLiftPercentageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWindowCoveringClusterGoToLiftPercentageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams => mtrWindowCoveringClusterGoToLiftPercentageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWindowCoveringClusterGoToLiftPercentageParams  =
    sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWindowCoveringClusterGoToLiftPercentageParams mtrWindowCoveringClusterGoToLiftPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToLiftPercentageParams -> value -> IO ()
setServerSideProcessingTimeout mtrWindowCoveringClusterGoToLiftPercentageParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWindowCoveringClusterGoToLiftPercentageParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @liftPercent100thsValue@
liftPercent100thsValueSelector :: Selector
liftPercent100thsValueSelector = mkSelector "liftPercent100thsValue"

-- | @Selector@ for @setLiftPercent100thsValue:@
setLiftPercent100thsValueSelector :: Selector
setLiftPercent100thsValueSelector = mkSelector "setLiftPercent100thsValue:"

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

