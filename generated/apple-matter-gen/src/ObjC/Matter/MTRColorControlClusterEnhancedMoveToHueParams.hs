{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterEnhancedMoveToHueParams@.
module ObjC.Matter.MTRColorControlClusterEnhancedMoveToHueParams
  ( MTRColorControlClusterEnhancedMoveToHueParams
  , IsMTRColorControlClusterEnhancedMoveToHueParams(..)
  , enhancedHue
  , setEnhancedHue
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
  , enhancedHueSelector
  , setEnhancedHueSelector
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

-- | @- enhancedHue@
enhancedHue :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
enhancedHue mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "enhancedHue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnhancedHue:@
setEnhancedHue :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setEnhancedHue mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setEnhancedHue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- direction@
direction :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
direction mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "direction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setDirection mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setDirection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueParams  =
    sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterEnhancedMoveToHueParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enhancedHue@
enhancedHueSelector :: Selector
enhancedHueSelector = mkSelector "enhancedHue"

-- | @Selector@ for @setEnhancedHue:@
setEnhancedHueSelector :: Selector
setEnhancedHueSelector = mkSelector "setEnhancedHue:"

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

