{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchResponseParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchResponseParams
  ( MTRContentLauncherClusterLaunchResponseParams
  , IsMTRContentLauncherClusterLaunchResponseParams(..)
  , status
  , setStatus
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , statusSelector
  , setStatusSelector
  , dataSelector
  , setDataSelector
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

-- | @- status@
status :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSNumber)
status mtrContentLauncherClusterLaunchResponseParams  =
    sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSNumber value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setStatus mtrContentLauncherClusterLaunchResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSString)
data_ mtrContentLauncherClusterLaunchResponseParams  =
    sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSString value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setData mtrContentLauncherClusterLaunchResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchResponseParams  =
    sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSNumber value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

