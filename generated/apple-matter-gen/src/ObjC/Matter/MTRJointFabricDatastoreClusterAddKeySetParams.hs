{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddKeySetParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddKeySetParams
  ( MTRJointFabricDatastoreClusterAddKeySetParams
  , IsMTRJointFabricDatastoreClusterAddKeySetParams(..)
  , groupKeySet
  , setGroupKeySet
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupKeySetSelector
  , setGroupKeySetSelector
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

-- | @- groupKeySet@
groupKeySet :: IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams => mtrJointFabricDatastoreClusterAddKeySetParams -> IO (Id MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct)
groupKeySet mtrJointFabricDatastoreClusterAddKeySetParams  =
    sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "groupKeySet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySet:@
setGroupKeySet :: (IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams, IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct value) => mtrJointFabricDatastoreClusterAddKeySetParams -> value -> IO ()
setGroupKeySet mtrJointFabricDatastoreClusterAddKeySetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "setGroupKeySet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams => mtrJointFabricDatastoreClusterAddKeySetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddKeySetParams  =
    sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddKeySetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddKeySetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams => mtrJointFabricDatastoreClusterAddKeySetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddKeySetParams  =
    sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddKeySetParams mtrJointFabricDatastoreClusterAddKeySetParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddKeySetParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddKeySetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterAddKeySetParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupKeySet@
groupKeySetSelector :: Selector
groupKeySetSelector = mkSelector "groupKeySet"

-- | @Selector@ for @setGroupKeySet:@
setGroupKeySetSelector :: Selector
setGroupKeySetSelector = mkSelector "setGroupKeySet:"

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

