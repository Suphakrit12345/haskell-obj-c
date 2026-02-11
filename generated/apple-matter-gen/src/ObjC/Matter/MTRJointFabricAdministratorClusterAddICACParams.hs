{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterAddICACParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterAddICACParams
  ( MTRJointFabricAdministratorClusterAddICACParams
  , IsMTRJointFabricAdministratorClusterAddICACParams(..)
  , icacValue
  , setIcacValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , icacValueSelector
  , setIcacValueSelector
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

-- | @- icacValue@
icacValue :: IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams => mtrJointFabricAdministratorClusterAddICACParams -> IO (Id NSData)
icacValue mtrJointFabricAdministratorClusterAddICACParams  =
    sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "icacValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcacValue:@
setIcacValue :: (IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams, IsNSData value) => mtrJointFabricAdministratorClusterAddICACParams -> value -> IO ()
setIcacValue mtrJointFabricAdministratorClusterAddICACParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "setIcacValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams => mtrJointFabricAdministratorClusterAddICACParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricAdministratorClusterAddICACParams  =
    sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams, IsNSNumber value) => mtrJointFabricAdministratorClusterAddICACParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricAdministratorClusterAddICACParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams => mtrJointFabricAdministratorClusterAddICACParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricAdministratorClusterAddICACParams  =
    sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricAdministratorClusterAddICACParams mtrJointFabricAdministratorClusterAddICACParams, IsNSNumber value) => mtrJointFabricAdministratorClusterAddICACParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricAdministratorClusterAddICACParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterAddICACParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @icacValue@
icacValueSelector :: Selector
icacValueSelector = mkSelector "icacValue"

-- | @Selector@ for @setIcacValue:@
setIcacValueSelector :: Selector
setIcacValueSelector = mkSelector "setIcacValue:"

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

