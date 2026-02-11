{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterUpdateGroupKeyParams@.
module ObjC.Matter.MTRGroupcastClusterUpdateGroupKeyParams
  ( MTRGroupcastClusterUpdateGroupKeyParams
  , IsMTRGroupcastClusterUpdateGroupKeyParams(..)
  , groupID
  , setGroupID
  , keyID
  , setKeyID
  , key
  , setKey
  , gracePeriod
  , setGracePeriod
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , setGroupIDSelector
  , keyIDSelector
  , setKeyIDSelector
  , keySelector
  , setKeySelector
  , gracePeriodSelector
  , setGracePeriodSelector
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

-- | @- groupID@
groupID :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setGroupID mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyID@
keyID :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
keyID mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "keyID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setKeyID mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setKeyID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- key@
key :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSData)
key mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSData value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setKey mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gracePeriod@
gracePeriod :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
gracePeriod mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "gracePeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGracePeriod:@
setGracePeriod :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setGracePeriod mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setGracePeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterUpdateGroupKeyParams  =
    sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterUpdateGroupKeyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterUpdateGroupKeyParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @keyID@
keyIDSelector :: Selector
keyIDSelector = mkSelector "keyID"

-- | @Selector@ for @setKeyID:@
setKeyIDSelector :: Selector
setKeyIDSelector = mkSelector "setKeyID:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @gracePeriod@
gracePeriodSelector :: Selector
gracePeriodSelector = mkSelector "gracePeriod"

-- | @Selector@ for @setGracePeriod:@
setGracePeriodSelector :: Selector
setGracePeriodSelector = mkSelector "setGracePeriod:"

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

