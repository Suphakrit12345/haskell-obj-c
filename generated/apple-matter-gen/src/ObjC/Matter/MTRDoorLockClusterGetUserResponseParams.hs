{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetUserResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetUserResponseParams
  ( MTRDoorLockClusterGetUserResponseParams
  , IsMTRDoorLockClusterGetUserResponseParams(..)
  , initWithResponseValue_error
  , userIndex
  , setUserIndex
  , userName
  , setUserName
  , userUniqueID
  , setUserUniqueID
  , userStatus
  , setUserStatus
  , userType
  , setUserType
  , credentialRule
  , setCredentialRule
  , credentials
  , setCredentials
  , creatorFabricIndex
  , setCreatorFabricIndex
  , lastModifiedFabricIndex
  , setLastModifiedFabricIndex
  , nextUserIndex
  , setNextUserIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , userUniqueId
  , setUserUniqueId
  , initWithResponseValue_errorSelector
  , userIndexSelector
  , setUserIndexSelector
  , userNameSelector
  , setUserNameSelector
  , userUniqueIDSelector
  , setUserUniqueIDSelector
  , userStatusSelector
  , setUserStatusSelector
  , userTypeSelector
  , setUserTypeSelector
  , credentialRuleSelector
  , setCredentialRuleSelector
  , credentialsSelector
  , setCredentialsSelector
  , creatorFabricIndexSelector
  , setCreatorFabricIndexSelector
  , lastModifiedFabricIndexSelector
  , setLastModifiedFabricIndexSelector
  , nextUserIndexSelector
  , setNextUserIndexSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , userUniqueIdSelector
  , setUserUniqueIdSelector


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

-- | Initialize an MTRDoorLockClusterGetUserResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetUserResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetUserResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetUserResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userName@
userName :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSString)
userName mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserName:@
setUserName :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSString value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserName mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userUniqueID@
userUniqueID :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userUniqueID mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userUniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserUniqueID:@
setUserUniqueID :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserUniqueID mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserStatus mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userType@
userType :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userType mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserType mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialRule@
credentialRule :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
credentialRule mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "credentialRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialRule:@
setCredentialRule :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCredentialRule mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setCredentialRule:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSArray)
credentials mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSArray value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCredentials mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- creatorFabricIndex@
creatorFabricIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
creatorFabricIndex mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "creatorFabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreatorFabricIndex:@
setCreatorFabricIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCreatorFabricIndex mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setCreatorFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastModifiedFabricIndex@
lastModifiedFabricIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
lastModifiedFabricIndex mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "lastModifiedFabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastModifiedFabricIndex:@
setLastModifiedFabricIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setLastModifiedFabricIndex mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setLastModifiedFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextUserIndex@
nextUserIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
nextUserIndex mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "nextUserIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextUserIndex:@
setNextUserIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setNextUserIndex mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setNextUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userUniqueId@
userUniqueId :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userUniqueId mtrDoorLockClusterGetUserResponseParams  =
    sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "userUniqueId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserUniqueId:@
setUserUniqueId :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserUniqueId mtrDoorLockClusterGetUserResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetUserResponseParams (mkSelector "setUserUniqueId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @userName@
userNameSelector :: Selector
userNameSelector = mkSelector "userName"

-- | @Selector@ for @setUserName:@
setUserNameSelector :: Selector
setUserNameSelector = mkSelector "setUserName:"

-- | @Selector@ for @userUniqueID@
userUniqueIDSelector :: Selector
userUniqueIDSelector = mkSelector "userUniqueID"

-- | @Selector@ for @setUserUniqueID:@
setUserUniqueIDSelector :: Selector
setUserUniqueIDSelector = mkSelector "setUserUniqueID:"

-- | @Selector@ for @userStatus@
userStatusSelector :: Selector
userStatusSelector = mkSelector "userStatus"

-- | @Selector@ for @setUserStatus:@
setUserStatusSelector :: Selector
setUserStatusSelector = mkSelector "setUserStatus:"

-- | @Selector@ for @userType@
userTypeSelector :: Selector
userTypeSelector = mkSelector "userType"

-- | @Selector@ for @setUserType:@
setUserTypeSelector :: Selector
setUserTypeSelector = mkSelector "setUserType:"

-- | @Selector@ for @credentialRule@
credentialRuleSelector :: Selector
credentialRuleSelector = mkSelector "credentialRule"

-- | @Selector@ for @setCredentialRule:@
setCredentialRuleSelector :: Selector
setCredentialRuleSelector = mkSelector "setCredentialRule:"

-- | @Selector@ for @credentials@
credentialsSelector :: Selector
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector
setCredentialsSelector = mkSelector "setCredentials:"

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

-- | @Selector@ for @nextUserIndex@
nextUserIndexSelector :: Selector
nextUserIndexSelector = mkSelector "nextUserIndex"

-- | @Selector@ for @setNextUserIndex:@
setNextUserIndexSelector :: Selector
setNextUserIndexSelector = mkSelector "setNextUserIndex:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @userUniqueId@
userUniqueIdSelector :: Selector
userUniqueIdSelector = mkSelector "userUniqueId"

-- | @Selector@ for @setUserUniqueId:@
setUserUniqueIdSelector :: Selector
setUserUniqueIdSelector = mkSelector "setUserUniqueId:"

