{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterStopWithOnOffParams@.
module ObjC.Matter.MTRLevelControlClusterStopWithOnOffParams
  ( MTRLevelControlClusterStopWithOnOffParams
  , IsMTRLevelControlClusterStopWithOnOffParams(..)
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams => mtrLevelControlClusterStopWithOnOffParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterStopWithOnOffParams  =
    sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterStopWithOnOffParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterStopWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams => mtrLevelControlClusterStopWithOnOffParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterStopWithOnOffParams  =
    sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterStopWithOnOffParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterStopWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams => mtrLevelControlClusterStopWithOnOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterStopWithOnOffParams  =
    sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterStopWithOnOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterStopWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams => mtrLevelControlClusterStopWithOnOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterStopWithOnOffParams  =
    sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterStopWithOnOffParams mtrLevelControlClusterStopWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterStopWithOnOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterStopWithOnOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLevelControlClusterStopWithOnOffParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

