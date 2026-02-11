{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterEcosystemDeviceStruct@.
module ObjC.Matter.MTREcosystemInformationClusterEcosystemDeviceStruct
  ( MTREcosystemInformationClusterEcosystemDeviceStruct
  , IsMTREcosystemInformationClusterEcosystemDeviceStruct(..)
  , deviceName
  , setDeviceName
  , deviceNameLastEdit
  , setDeviceNameLastEdit
  , bridgedEndpoint
  , setBridgedEndpoint
  , originalEndpoint
  , setOriginalEndpoint
  , deviceTypes
  , setDeviceTypes
  , uniqueLocationIDs
  , setUniqueLocationIDs
  , uniqueLocationIDsLastEdit
  , setUniqueLocationIDsLastEdit
  , fabricIndex
  , setFabricIndex
  , deviceNameSelector
  , setDeviceNameSelector
  , deviceNameLastEditSelector
  , setDeviceNameLastEditSelector
  , bridgedEndpointSelector
  , setBridgedEndpointSelector
  , originalEndpointSelector
  , setOriginalEndpointSelector
  , deviceTypesSelector
  , setDeviceTypesSelector
  , uniqueLocationIDsSelector
  , setUniqueLocationIDsSelector
  , uniqueLocationIDsLastEditSelector
  , setUniqueLocationIDsLastEditSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- deviceName@
deviceName :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSString)
deviceName mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "deviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceName:@
setDeviceName :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSString value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceName mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setDeviceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deviceNameLastEdit@
deviceNameLastEdit :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
deviceNameLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "deviceNameLastEdit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceNameLastEdit:@
setDeviceNameLastEdit :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceNameLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setDeviceNameLastEdit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bridgedEndpoint@
bridgedEndpoint :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
bridgedEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "bridgedEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBridgedEndpoint:@
setBridgedEndpoint :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setBridgedEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setBridgedEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originalEndpoint@
originalEndpoint :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
originalEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "originalEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalEndpoint:@
setOriginalEndpoint :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setOriginalEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setOriginalEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deviceTypes@
deviceTypes :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSArray)
deviceTypes mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "deviceTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceTypes:@
setDeviceTypes :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSArray value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceTypes mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setDeviceTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uniqueLocationIDs@
uniqueLocationIDs :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSArray)
uniqueLocationIDs mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "uniqueLocationIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueLocationIDs:@
setUniqueLocationIDs :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSArray value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setUniqueLocationIDs mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setUniqueLocationIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uniqueLocationIDsLastEdit@
uniqueLocationIDsLastEdit :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
uniqueLocationIDsLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "uniqueLocationIDsLastEdit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueLocationIDsLastEdit:@
setUniqueLocationIDsLastEdit :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setUniqueLocationIDsLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setUniqueLocationIDsLastEdit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
fabricIndex mtrEcosystemInformationClusterEcosystemDeviceStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setFabricIndex mtrEcosystemInformationClusterEcosystemDeviceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemDeviceStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector
deviceNameSelector = mkSelector "deviceName"

-- | @Selector@ for @setDeviceName:@
setDeviceNameSelector :: Selector
setDeviceNameSelector = mkSelector "setDeviceName:"

-- | @Selector@ for @deviceNameLastEdit@
deviceNameLastEditSelector :: Selector
deviceNameLastEditSelector = mkSelector "deviceNameLastEdit"

-- | @Selector@ for @setDeviceNameLastEdit:@
setDeviceNameLastEditSelector :: Selector
setDeviceNameLastEditSelector = mkSelector "setDeviceNameLastEdit:"

-- | @Selector@ for @bridgedEndpoint@
bridgedEndpointSelector :: Selector
bridgedEndpointSelector = mkSelector "bridgedEndpoint"

-- | @Selector@ for @setBridgedEndpoint:@
setBridgedEndpointSelector :: Selector
setBridgedEndpointSelector = mkSelector "setBridgedEndpoint:"

-- | @Selector@ for @originalEndpoint@
originalEndpointSelector :: Selector
originalEndpointSelector = mkSelector "originalEndpoint"

-- | @Selector@ for @setOriginalEndpoint:@
setOriginalEndpointSelector :: Selector
setOriginalEndpointSelector = mkSelector "setOriginalEndpoint:"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @setDeviceTypes:@
setDeviceTypesSelector :: Selector
setDeviceTypesSelector = mkSelector "setDeviceTypes:"

-- | @Selector@ for @uniqueLocationIDs@
uniqueLocationIDsSelector :: Selector
uniqueLocationIDsSelector = mkSelector "uniqueLocationIDs"

-- | @Selector@ for @setUniqueLocationIDs:@
setUniqueLocationIDsSelector :: Selector
setUniqueLocationIDsSelector = mkSelector "setUniqueLocationIDs:"

-- | @Selector@ for @uniqueLocationIDsLastEdit@
uniqueLocationIDsLastEditSelector :: Selector
uniqueLocationIDsLastEditSelector = mkSelector "uniqueLocationIDsLastEdit"

-- | @Selector@ for @setUniqueLocationIDsLastEdit:@
setUniqueLocationIDsLastEditSelector :: Selector
setUniqueLocationIDsLastEditSelector = mkSelector "setUniqueLocationIDsLastEdit:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

