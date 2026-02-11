{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterMoveToParams@.
module ObjC.Matter.MTRClosureControlClusterMoveToParams
  ( MTRClosureControlClusterMoveToParams
  , IsMTRClosureControlClusterMoveToParams(..)
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
position :: IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams => mtrClosureControlClusterMoveToParams -> IO (Id NSNumber)
position mtrClosureControlClusterMoveToParams  =
    sendMsg mtrClosureControlClusterMoveToParams (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams, IsNSNumber value) => mtrClosureControlClusterMoveToParams -> value -> IO ()
setPosition mtrClosureControlClusterMoveToParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterMoveToParams (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latch@
latch :: IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams => mtrClosureControlClusterMoveToParams -> IO (Id NSNumber)
latch mtrClosureControlClusterMoveToParams  =
    sendMsg mtrClosureControlClusterMoveToParams (mkSelector "latch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatch:@
setLatch :: (IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams, IsNSNumber value) => mtrClosureControlClusterMoveToParams -> value -> IO ()
setLatch mtrClosureControlClusterMoveToParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterMoveToParams (mkSelector "setLatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams => mtrClosureControlClusterMoveToParams -> IO (Id NSNumber)
speed mtrClosureControlClusterMoveToParams  =
    sendMsg mtrClosureControlClusterMoveToParams (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams, IsNSNumber value) => mtrClosureControlClusterMoveToParams -> value -> IO ()
setSpeed mtrClosureControlClusterMoveToParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterMoveToParams (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams => mtrClosureControlClusterMoveToParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrClosureControlClusterMoveToParams  =
    sendMsg mtrClosureControlClusterMoveToParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams, IsNSNumber value) => mtrClosureControlClusterMoveToParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrClosureControlClusterMoveToParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterMoveToParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams => mtrClosureControlClusterMoveToParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrClosureControlClusterMoveToParams  =
    sendMsg mtrClosureControlClusterMoveToParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRClosureControlClusterMoveToParams mtrClosureControlClusterMoveToParams, IsNSNumber value) => mtrClosureControlClusterMoveToParams -> value -> IO ()
setServerSideProcessingTimeout mtrClosureControlClusterMoveToParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterMoveToParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

