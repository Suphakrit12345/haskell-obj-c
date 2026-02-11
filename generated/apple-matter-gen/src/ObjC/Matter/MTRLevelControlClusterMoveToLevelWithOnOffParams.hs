{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterMoveToLevelWithOnOffParams@.
module ObjC.Matter.MTRLevelControlClusterMoveToLevelWithOnOffParams
  ( MTRLevelControlClusterMoveToLevelWithOnOffParams
  , IsMTRLevelControlClusterMoveToLevelWithOnOffParams(..)
  , level
  , setLevel
  , transitionTime
  , setTransitionTime
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , levelSelector
  , setLevelSelector
  , transitionTimeSelector
  , setTransitionTimeSelector
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

-- | @- level@
level :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
level mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "level") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLevel:@
setLevel :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setLevel mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterMoveToLevelWithOnOffParams  =
    sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterMoveToLevelWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelWithOnOffParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector
setTransitionTimeSelector = mkSelector "setTransitionTime:"

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

