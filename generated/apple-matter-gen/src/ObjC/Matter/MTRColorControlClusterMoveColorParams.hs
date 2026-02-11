{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveColorParams@.
module ObjC.Matter.MTRColorControlClusterMoveColorParams
  ( MTRColorControlClusterMoveColorParams
  , IsMTRColorControlClusterMoveColorParams(..)
  , rateX
  , setRateX
  , rateY
  , setRateY
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , rateXSelector
  , setRateXSelector
  , rateYSelector
  , setRateYSelector
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

-- | @- rateX@
rateX :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
rateX mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "rateX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRateX:@
setRateX :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setRateX mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setRateX:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rateY@
rateY :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
rateY mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "rateY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRateY:@
setRateY :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setRateY mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setRateY:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveColorParams  =
    sendMsg mtrColorControlClusterMoveColorParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveColorParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rateX@
rateXSelector :: Selector
rateXSelector = mkSelector "rateX"

-- | @Selector@ for @setRateX:@
setRateXSelector :: Selector
setRateXSelector = mkSelector "setRateX:"

-- | @Selector@ for @rateY@
rateYSelector :: Selector
rateYSelector = mkSelector "rateY"

-- | @Selector@ for @setRateY:@
setRateYSelector :: Selector
setRateYSelector = mkSelector "setRateY:"

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

