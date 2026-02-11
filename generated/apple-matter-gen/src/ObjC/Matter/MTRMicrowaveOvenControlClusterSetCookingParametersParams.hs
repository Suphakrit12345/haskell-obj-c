{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenControlClusterSetCookingParametersParams@.
module ObjC.Matter.MTRMicrowaveOvenControlClusterSetCookingParametersParams
  ( MTRMicrowaveOvenControlClusterSetCookingParametersParams
  , IsMTRMicrowaveOvenControlClusterSetCookingParametersParams(..)
  , cookMode
  , setCookMode
  , cookTime
  , setCookTime
  , powerSetting
  , setPowerSetting
  , wattSettingIndex
  , setWattSettingIndex
  , startAfterSetting
  , setStartAfterSetting
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , cookModeSelector
  , setCookModeSelector
  , cookTimeSelector
  , setCookTimeSelector
  , powerSettingSelector
  , setPowerSettingSelector
  , wattSettingIndexSelector
  , setWattSettingIndexSelector
  , startAfterSettingSelector
  , setStartAfterSettingSelector
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

-- | @- cookMode@
cookMode :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
cookMode mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "cookMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCookMode:@
setCookMode :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setCookMode mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setCookMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cookTime@
cookTime :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
cookTime mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "cookTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCookTime:@
setCookTime :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setCookTime mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setCookTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- powerSetting@
powerSetting :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
powerSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "powerSetting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPowerSetting:@
setPowerSetting :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setPowerSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setPowerSetting:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wattSettingIndex@
wattSettingIndex :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
wattSettingIndex mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "wattSettingIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWattSettingIndex:@
setWattSettingIndex :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setWattSettingIndex mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setWattSettingIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startAfterSetting@
startAfterSetting :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
startAfterSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "startAfterSetting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartAfterSetting:@
setStartAfterSetting :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setStartAfterSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setStartAfterSetting:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMicrowaveOvenControlClusterSetCookingParametersParams  =
    sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setServerSideProcessingTimeout mtrMicrowaveOvenControlClusterSetCookingParametersParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenControlClusterSetCookingParametersParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cookMode@
cookModeSelector :: Selector
cookModeSelector = mkSelector "cookMode"

-- | @Selector@ for @setCookMode:@
setCookModeSelector :: Selector
setCookModeSelector = mkSelector "setCookMode:"

-- | @Selector@ for @cookTime@
cookTimeSelector :: Selector
cookTimeSelector = mkSelector "cookTime"

-- | @Selector@ for @setCookTime:@
setCookTimeSelector :: Selector
setCookTimeSelector = mkSelector "setCookTime:"

-- | @Selector@ for @powerSetting@
powerSettingSelector :: Selector
powerSettingSelector = mkSelector "powerSetting"

-- | @Selector@ for @setPowerSetting:@
setPowerSettingSelector :: Selector
setPowerSettingSelector = mkSelector "setPowerSetting:"

-- | @Selector@ for @wattSettingIndex@
wattSettingIndexSelector :: Selector
wattSettingIndexSelector = mkSelector "wattSettingIndex"

-- | @Selector@ for @setWattSettingIndex:@
setWattSettingIndexSelector :: Selector
setWattSettingIndexSelector = mkSelector "setWattSettingIndex:"

-- | @Selector@ for @startAfterSetting@
startAfterSettingSelector :: Selector
startAfterSettingSelector = mkSelector "startAfterSetting"

-- | @Selector@ for @setStartAfterSetting:@
setStartAfterSettingSelector :: Selector
setStartAfterSettingSelector = mkSelector "setStartAfterSetting:"

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

