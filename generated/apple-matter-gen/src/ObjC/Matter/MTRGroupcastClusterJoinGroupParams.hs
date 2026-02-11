{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterJoinGroupParams@.
module ObjC.Matter.MTRGroupcastClusterJoinGroupParams
  ( MTRGroupcastClusterJoinGroupParams
  , IsMTRGroupcastClusterJoinGroupParams(..)
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , keyID
  , setKeyID
  , key
  , setKey
  , gracePeriod
  , setGracePeriod
  , useAuxiliaryACL
  , setUseAuxiliaryACL
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , setGroupIDSelector
  , endpointsSelector
  , setEndpointsSelector
  , keyIDSelector
  , setKeyIDSelector
  , keySelector
  , setKeySelector
  , gracePeriodSelector
  , setGracePeriodSelector
  , useAuxiliaryACLSelector
  , setUseAuxiliaryACLSelector
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
groupID :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setGroupID mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSArray)
endpoints mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "endpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSArray value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setEndpoints mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setEndpoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyID@
keyID :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
keyID mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "keyID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setKeyID mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setKeyID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- key@
key :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSData)
key mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSData value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setKey mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gracePeriod@
gracePeriod :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
gracePeriod mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "gracePeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGracePeriod:@
setGracePeriod :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setGracePeriod mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setGracePeriod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- useAuxiliaryACL@
useAuxiliaryACL :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
useAuxiliaryACL mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "useAuxiliaryACL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUseAuxiliaryACL:@
setUseAuxiliaryACL :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setUseAuxiliaryACL mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setUseAuxiliaryACL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterJoinGroupParams  =
    sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterJoinGroupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterJoinGroupParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector
setEndpointsSelector = mkSelector "setEndpoints:"

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

-- | @Selector@ for @useAuxiliaryACL@
useAuxiliaryACLSelector :: Selector
useAuxiliaryACLSelector = mkSelector "useAuxiliaryACL"

-- | @Selector@ for @setUseAuxiliaryACL:@
setUseAuxiliaryACLSelector :: Selector
setUseAuxiliaryACLSelector = mkSelector "setUseAuxiliaryACL:"

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

