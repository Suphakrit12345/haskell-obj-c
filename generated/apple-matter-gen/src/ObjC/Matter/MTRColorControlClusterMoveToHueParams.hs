{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToHueParams@.
module ObjC.Matter.MTRColorControlClusterMoveToHueParams
  ( MTRColorControlClusterMoveToHueParams
  , IsMTRColorControlClusterMoveToHueParams(..)
  , hue
  , setHue
  , direction
  , setDirection
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
  , hueSelector
  , setHueSelector
  , directionSelector
  , setDirectionSelector
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

-- | @- hue@
hue :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
hue mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "hue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHue:@
setHue :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setHue mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setHue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- direction@
direction :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
direction mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "direction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setDirection mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setDirection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToHueParams  =
    sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hue@
hueSelector :: Selector
hueSelector = mkSelector "hue"

-- | @Selector@ for @setHue:@
setHueSelector :: Selector
setHueSelector = mkSelector "setHue:"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

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

