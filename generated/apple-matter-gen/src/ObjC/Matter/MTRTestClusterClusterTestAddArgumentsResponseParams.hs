{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestAddArgumentsResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestAddArgumentsResponseParams
  ( MTRTestClusterClusterTestAddArgumentsResponseParams
  , IsMTRTestClusterClusterTestAddArgumentsResponseParams(..)
  , returnValue
  , setReturnValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , returnValueSelector
  , setReturnValueSelector
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

-- | @- returnValue@
returnValue :: IsMTRTestClusterClusterTestAddArgumentsResponseParams mtrTestClusterClusterTestAddArgumentsResponseParams => mtrTestClusterClusterTestAddArgumentsResponseParams -> IO (Id NSNumber)
returnValue mtrTestClusterClusterTestAddArgumentsResponseParams  =
    sendMsg mtrTestClusterClusterTestAddArgumentsResponseParams (mkSelector "returnValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReturnValue:@
setReturnValue :: (IsMTRTestClusterClusterTestAddArgumentsResponseParams mtrTestClusterClusterTestAddArgumentsResponseParams, IsNSNumber value) => mtrTestClusterClusterTestAddArgumentsResponseParams -> value -> IO ()
setReturnValue mtrTestClusterClusterTestAddArgumentsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestAddArgumentsResponseParams (mkSelector "setReturnValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestAddArgumentsResponseParams mtrTestClusterClusterTestAddArgumentsResponseParams => mtrTestClusterClusterTestAddArgumentsResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestAddArgumentsResponseParams  =
    sendMsg mtrTestClusterClusterTestAddArgumentsResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestAddArgumentsResponseParams mtrTestClusterClusterTestAddArgumentsResponseParams, IsNSNumber value) => mtrTestClusterClusterTestAddArgumentsResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestAddArgumentsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestAddArgumentsResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

