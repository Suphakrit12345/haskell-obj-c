{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterMoveToLevelParams@.
module ObjC.Matter.MTRLevelControlClusterMoveToLevelParams
  ( MTRLevelControlClusterMoveToLevelParams
  , IsMTRLevelControlClusterMoveToLevelParams(..)
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
level :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
level mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "level") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLevel:@
setLevel :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setLevel mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterMoveToLevelParams  =
    sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterMoveToLevelParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterMoveToLevelParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

