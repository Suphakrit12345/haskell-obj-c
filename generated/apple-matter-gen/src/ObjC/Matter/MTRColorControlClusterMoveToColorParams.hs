{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToColorParams@.
module ObjC.Matter.MTRColorControlClusterMoveToColorParams
  ( MTRColorControlClusterMoveToColorParams
  , IsMTRColorControlClusterMoveToColorParams(..)
  , colorX
  , setColorX
  , colorY
  , setColorY
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
  , colorXSelector
  , setColorXSelector
  , colorYSelector
  , setColorYSelector
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

-- | @- colorX@
colorX :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
colorX mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "colorX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorX:@
setColorX :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setColorX mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setColorX:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorY@
colorY :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
colorY mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "colorY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorY:@
setColorY :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setColorY mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setColorY:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToColorParams  =
    sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToColorParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToColorParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorX@
colorXSelector :: Selector
colorXSelector = mkSelector "colorX"

-- | @Selector@ for @setColorX:@
setColorXSelector :: Selector
setColorXSelector = mkSelector "setColorX:"

-- | @Selector@ for @colorY@
colorYSelector :: Selector
colorYSelector = mkSelector "colorY"

-- | @Selector@ for @setColorY:@
setColorYSelector :: Selector
setColorYSelector = mkSelector "setColorY:"

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

