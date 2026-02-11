{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams
  ( MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams
  , IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , statusText
  , setStatusText
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , statusTextSelector
  , setStatusTextSelector


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

-- | Initialize an MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams  =
    sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams  =
    sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSString value) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @statusText@
statusTextSelector :: Selector
statusTextSelector = mkSelector "statusText"

-- | @Selector@ for @setStatusText:@
setStatusTextSelector :: Selector
setStatusTextSelector = mkSelector "setStatusText:"

