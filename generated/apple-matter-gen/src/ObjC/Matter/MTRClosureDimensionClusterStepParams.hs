{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterStepParams@.
module ObjC.Matter.MTRClosureDimensionClusterStepParams
  ( MTRClosureDimensionClusterStepParams
  , IsMTRClosureDimensionClusterStepParams(..)
  , direction
  , setDirection
  , numberOfSteps
  , setNumberOfSteps
  , speed
  , setSpeed
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , directionSelector
  , setDirectionSelector
  , numberOfStepsSelector
  , setNumberOfStepsSelector
  , speedSelector
  , setSpeedSelector
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

-- | @- direction@
direction :: IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams => mtrClosureDimensionClusterStepParams -> IO (Id NSNumber)
direction mtrClosureDimensionClusterStepParams  =
    sendMsg mtrClosureDimensionClusterStepParams (mkSelector "direction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirection:@
setDirection :: (IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams, IsNSNumber value) => mtrClosureDimensionClusterStepParams -> value -> IO ()
setDirection mtrClosureDimensionClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterStepParams (mkSelector "setDirection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfSteps@
numberOfSteps :: IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams => mtrClosureDimensionClusterStepParams -> IO (Id NSNumber)
numberOfSteps mtrClosureDimensionClusterStepParams  =
    sendMsg mtrClosureDimensionClusterStepParams (mkSelector "numberOfSteps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfSteps:@
setNumberOfSteps :: (IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams, IsNSNumber value) => mtrClosureDimensionClusterStepParams -> value -> IO ()
setNumberOfSteps mtrClosureDimensionClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterStepParams (mkSelector "setNumberOfSteps:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams => mtrClosureDimensionClusterStepParams -> IO (Id NSNumber)
speed mtrClosureDimensionClusterStepParams  =
    sendMsg mtrClosureDimensionClusterStepParams (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams, IsNSNumber value) => mtrClosureDimensionClusterStepParams -> value -> IO ()
setSpeed mtrClosureDimensionClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterStepParams (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams => mtrClosureDimensionClusterStepParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrClosureDimensionClusterStepParams  =
    sendMsg mtrClosureDimensionClusterStepParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams, IsNSNumber value) => mtrClosureDimensionClusterStepParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrClosureDimensionClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterStepParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams => mtrClosureDimensionClusterStepParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrClosureDimensionClusterStepParams  =
    sendMsg mtrClosureDimensionClusterStepParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRClosureDimensionClusterStepParams mtrClosureDimensionClusterStepParams, IsNSNumber value) => mtrClosureDimensionClusterStepParams -> value -> IO ()
setServerSideProcessingTimeout mtrClosureDimensionClusterStepParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterStepParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @numberOfSteps@
numberOfStepsSelector :: Selector
numberOfStepsSelector = mkSelector "numberOfSteps"

-- | @Selector@ for @setNumberOfSteps:@
setNumberOfStepsSelector :: Selector
setNumberOfStepsSelector = mkSelector "setNumberOfSteps:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

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

