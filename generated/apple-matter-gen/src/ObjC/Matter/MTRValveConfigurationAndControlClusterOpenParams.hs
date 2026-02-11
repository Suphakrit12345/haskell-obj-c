{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterOpenParams@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterOpenParams
  ( MTRValveConfigurationAndControlClusterOpenParams
  , IsMTRValveConfigurationAndControlClusterOpenParams(..)
  , openDuration
  , setOpenDuration
  , targetLevel
  , setTargetLevel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , openDurationSelector
  , setOpenDurationSelector
  , targetLevelSelector
  , setTargetLevelSelector
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

-- | @- openDuration@
openDuration :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
openDuration mtrValveConfigurationAndControlClusterOpenParams  =
    sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "openDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOpenDuration:@
setOpenDuration :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setOpenDuration mtrValveConfigurationAndControlClusterOpenParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "setOpenDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetLevel@
targetLevel :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
targetLevel mtrValveConfigurationAndControlClusterOpenParams  =
    sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "targetLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetLevel:@
setTargetLevel :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setTargetLevel mtrValveConfigurationAndControlClusterOpenParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "setTargetLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrValveConfigurationAndControlClusterOpenParams  =
    sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrValveConfigurationAndControlClusterOpenParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrValveConfigurationAndControlClusterOpenParams  =
    sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setServerSideProcessingTimeout mtrValveConfigurationAndControlClusterOpenParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterOpenParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openDuration@
openDurationSelector :: Selector
openDurationSelector = mkSelector "openDuration"

-- | @Selector@ for @setOpenDuration:@
setOpenDurationSelector :: Selector
setOpenDurationSelector = mkSelector "setOpenDuration:"

-- | @Selector@ for @targetLevel@
targetLevelSelector :: Selector
targetLevelSelector = mkSelector "targetLevel"

-- | @Selector@ for @setTargetLevel:@
setTargetLevelSelector :: Selector
setTargetLevelSelector = mkSelector "setTargetLevel:"

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

