{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams@.
module ObjC.Matter.MTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams
  ( MTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams
  , IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams(..)
  , action
  , setAction
  , delayedActionTime
  , setDelayedActionTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , actionSelector
  , setActionSelector
  , delayedActionTimeSelector
  , setDelayedActionTimeSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector


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

-- | @- action@
action :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
action mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "action") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAction:@
setAction :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setAction mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "setAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delayedActionTime@
delayedActionTime :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
delayedActionTime mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "delayedActionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDelayedActionTime:@
setDelayedActionTime :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setDelayedActionTime mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "setDelayedActionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateResponseParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @delayedActionTime@
delayedActionTimeSelector :: Selector
delayedActionTimeSelector = mkSelector "delayedActionTime"

-- | @Selector@ for @setDelayedActionTime:@
setDelayedActionTimeSelector :: Selector
setDelayedActionTimeSelector = mkSelector "setDelayedActionTime:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

