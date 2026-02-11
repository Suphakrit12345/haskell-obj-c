{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetCredentialStatusResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetCredentialStatusResponseParams
  ( MTRDoorLockClusterGetCredentialStatusResponseParams
  , IsMTRDoorLockClusterGetCredentialStatusResponseParams(..)
  , initWithResponseValue_error
  , credentialExists
  , setCredentialExists
  , userIndex
  , setUserIndex
  , creatorFabricIndex
  , setCreatorFabricIndex
  , lastModifiedFabricIndex
  , setLastModifiedFabricIndex
  , nextCredentialIndex
  , setNextCredentialIndex
  , credentialData
  , setCredentialData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , credentialExistsSelector
  , setCredentialExistsSelector
  , userIndexSelector
  , setUserIndexSelector
  , creatorFabricIndexSelector
  , setCreatorFabricIndexSelector
  , lastModifiedFabricIndexSelector
  , setLastModifiedFabricIndexSelector
  , nextCredentialIndexSelector
  , setNextCredentialIndexSelector
  , credentialDataSelector
  , setCredentialDataSelector
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

-- | Initialize an MTRDoorLockClusterGetCredentialStatusResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetCredentialStatusResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetCredentialStatusResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetCredentialStatusResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- credentialExists@
credentialExists :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
credentialExists mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "credentialExists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialExists:@
setCredentialExists :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setCredentialExists mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setCredentialExists:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- creatorFabricIndex@
creatorFabricIndex :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
creatorFabricIndex mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "creatorFabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreatorFabricIndex:@
setCreatorFabricIndex :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setCreatorFabricIndex mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setCreatorFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastModifiedFabricIndex@
lastModifiedFabricIndex :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
lastModifiedFabricIndex mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "lastModifiedFabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastModifiedFabricIndex:@
setLastModifiedFabricIndex :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setLastModifiedFabricIndex mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setLastModifiedFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextCredentialIndex@
nextCredentialIndex :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
nextCredentialIndex mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "nextCredentialIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextCredentialIndex:@
setNextCredentialIndex :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setNextCredentialIndex mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setNextCredentialIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialData@
credentialData :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSData)
credentialData mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "credentialData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialData:@
setCredentialData :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSData value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setCredentialData mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setCredentialData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams => mtrDoorLockClusterGetCredentialStatusResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetCredentialStatusResponseParams  =
    sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetCredentialStatusResponseParams mtrDoorLockClusterGetCredentialStatusResponseParams, IsNSNumber value) => mtrDoorLockClusterGetCredentialStatusResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetCredentialStatusResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetCredentialStatusResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @credentialExists@
credentialExistsSelector :: Selector
credentialExistsSelector = mkSelector "credentialExists"

-- | @Selector@ for @setCredentialExists:@
setCredentialExistsSelector :: Selector
setCredentialExistsSelector = mkSelector "setCredentialExists:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @creatorFabricIndex@
creatorFabricIndexSelector :: Selector
creatorFabricIndexSelector = mkSelector "creatorFabricIndex"

-- | @Selector@ for @setCreatorFabricIndex:@
setCreatorFabricIndexSelector :: Selector
setCreatorFabricIndexSelector = mkSelector "setCreatorFabricIndex:"

-- | @Selector@ for @lastModifiedFabricIndex@
lastModifiedFabricIndexSelector :: Selector
lastModifiedFabricIndexSelector = mkSelector "lastModifiedFabricIndex"

-- | @Selector@ for @setLastModifiedFabricIndex:@
setLastModifiedFabricIndexSelector :: Selector
setLastModifiedFabricIndexSelector = mkSelector "setLastModifiedFabricIndex:"

-- | @Selector@ for @nextCredentialIndex@
nextCredentialIndexSelector :: Selector
nextCredentialIndexSelector = mkSelector "nextCredentialIndex"

-- | @Selector@ for @setNextCredentialIndex:@
setNextCredentialIndexSelector :: Selector
setNextCredentialIndexSelector = mkSelector "setNextCredentialIndex:"

-- | @Selector@ for @credentialData@
credentialDataSelector :: Selector
credentialDataSelector = mkSelector "credentialData"

-- | @Selector@ for @setCredentialData:@
setCredentialDataSelector :: Selector
setCredentialDataSelector = mkSelector "setCredentialData:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

