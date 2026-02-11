{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterSetTargetParams@.
module ObjC.Matter.MTRClosureDimensionClusterSetTargetParams
  ( MTRClosureDimensionClusterSetTargetParams
  , IsMTRClosureDimensionClusterSetTargetParams(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , positionSelector
  , setPositionSelector
  , latchSelector
  , setLatchSelector
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

-- | @- position@
position :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
position mtrClosureDimensionClusterSetTargetParams  =
    sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setPosition mtrClosureDimensionClusterSetTargetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latch@
latch :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
latch mtrClosureDimensionClusterSetTargetParams  =
    sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "latch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatch:@
setLatch :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setLatch mtrClosureDimensionClusterSetTargetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "setLatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
speed mtrClosureDimensionClusterSetTargetParams  =
    sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setSpeed mtrClosureDimensionClusterSetTargetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrClosureDimensionClusterSetTargetParams  =
    sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrClosureDimensionClusterSetTargetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrClosureDimensionClusterSetTargetParams  =
    sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setServerSideProcessingTimeout mtrClosureDimensionClusterSetTargetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterSetTargetParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @latch@
latchSelector :: Selector
latchSelector = mkSelector "latch"

-- | @Selector@ for @setLatch:@
setLatchSelector :: Selector
setLatchSelector = mkSelector "setLatch:"

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

