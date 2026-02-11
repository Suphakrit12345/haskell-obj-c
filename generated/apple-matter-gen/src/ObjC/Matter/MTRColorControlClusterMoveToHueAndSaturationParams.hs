{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToHueAndSaturationParams@.
module ObjC.Matter.MTRColorControlClusterMoveToHueAndSaturationParams
  ( MTRColorControlClusterMoveToHueAndSaturationParams
  , IsMTRColorControlClusterMoveToHueAndSaturationParams(..)
  , hue
  , setHue
  , saturation
  , setSaturation
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
  , saturationSelector
  , setSaturationSelector
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
hue :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
hue mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "hue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHue:@
setHue :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setHue mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setHue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- saturation@
saturation :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
saturation mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "saturation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSaturation:@
setSaturation :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setSaturation mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setSaturation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "optionsMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setOptionsMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "optionsOverride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setOptionsOverride:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToHueAndSaturationParams  =
    sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToHueAndSaturationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrColorControlClusterMoveToHueAndSaturationParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hue@
hueSelector :: Selector
hueSelector = mkSelector "hue"

-- | @Selector@ for @setHue:@
setHueSelector :: Selector
setHueSelector = mkSelector "setHue:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector
setSaturationSelector = mkSelector "setSaturation:"

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

