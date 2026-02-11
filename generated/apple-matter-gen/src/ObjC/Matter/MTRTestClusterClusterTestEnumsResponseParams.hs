{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEnumsResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestEnumsResponseParams
  ( MTRTestClusterClusterTestEnumsResponseParams
  , IsMTRTestClusterClusterTestEnumsResponseParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , arg1Selector
  , setArg1Selector
  , arg2Selector
  , setArg2Selector
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

-- | @- arg1@
arg1 :: IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams => mtrTestClusterClusterTestEnumsResponseParams -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEnumsResponseParams  =
    sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsResponseParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams => mtrTestClusterClusterTestEnumsResponseParams -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEnumsResponseParams  =
    sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsResponseParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams => mtrTestClusterClusterTestEnumsResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestEnumsResponseParams  =
    sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestEnumsResponseParams mtrTestClusterClusterTestEnumsResponseParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

