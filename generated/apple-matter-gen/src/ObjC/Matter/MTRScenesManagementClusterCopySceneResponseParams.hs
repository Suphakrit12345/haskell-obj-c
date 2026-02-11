{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterCopySceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterCopySceneResponseParams
  ( MTRScenesManagementClusterCopySceneResponseParams
  , IsMTRScenesManagementClusterCopySceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupIdentifierFrom
  , setGroupIdentifierFrom
  , sceneIdentifierFrom
  , setSceneIdentifierFrom
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIdentifierFromSelector
  , setGroupIdentifierFromSelector
  , sceneIdentifierFromSelector
  , setSceneIdentifierFromSelector


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

-- | Initialize an MTRScenesManagementClusterCopySceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterCopySceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterCopySceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterCopySceneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterCopySceneResponseParams  =
    sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterCopySceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifierFrom@
groupIdentifierFrom :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
groupIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams  =
    sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "groupIdentifierFrom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifierFrom:@
setGroupIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setGroupIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "setGroupIdentifierFrom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneIdentifierFrom@
sceneIdentifierFrom :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
sceneIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams  =
    sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "sceneIdentifierFrom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneIdentifierFrom:@
setSceneIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setSceneIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneResponseParams (mkSelector "setSceneIdentifierFrom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @groupIdentifierFrom@
groupIdentifierFromSelector :: Selector
groupIdentifierFromSelector = mkSelector "groupIdentifierFrom"

-- | @Selector@ for @setGroupIdentifierFrom:@
setGroupIdentifierFromSelector :: Selector
setGroupIdentifierFromSelector = mkSelector "setGroupIdentifierFrom:"

-- | @Selector@ for @sceneIdentifierFrom@
sceneIdentifierFromSelector :: Selector
sceneIdentifierFromSelector = mkSelector "sceneIdentifierFrom"

-- | @Selector@ for @setSceneIdentifierFrom:@
setSceneIdentifierFromSelector :: Selector
setSceneIdentifierFromSelector = mkSelector "setSceneIdentifierFrom:"

