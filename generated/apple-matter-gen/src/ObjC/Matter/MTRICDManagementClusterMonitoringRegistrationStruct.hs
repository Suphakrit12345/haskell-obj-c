{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterMonitoringRegistrationStruct@.
module ObjC.Matter.MTRICDManagementClusterMonitoringRegistrationStruct
  ( MTRICDManagementClusterMonitoringRegistrationStruct
  , IsMTRICDManagementClusterMonitoringRegistrationStruct(..)
  , checkInNodeID
  , setCheckInNodeID
  , monitoredSubject
  , setMonitoredSubject
  , clientType
  , setClientType
  , fabricIndex
  , setFabricIndex
  , checkInNodeIDSelector
  , setCheckInNodeIDSelector
  , monitoredSubjectSelector
  , setMonitoredSubjectSelector
  , clientTypeSelector
  , setClientTypeSelector
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

-- | @- checkInNodeID@
checkInNodeID :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
checkInNodeID mtricdManagementClusterMonitoringRegistrationStruct  =
    sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "checkInNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCheckInNodeID:@
setCheckInNodeID :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setCheckInNodeID mtricdManagementClusterMonitoringRegistrationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "setCheckInNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- monitoredSubject@
monitoredSubject :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
monitoredSubject mtricdManagementClusterMonitoringRegistrationStruct  =
    sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "monitoredSubject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMonitoredSubject:@
setMonitoredSubject :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setMonitoredSubject mtricdManagementClusterMonitoringRegistrationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "setMonitoredSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientType@
clientType :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
clientType mtricdManagementClusterMonitoringRegistrationStruct  =
    sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "clientType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientType:@
setClientType :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setClientType mtricdManagementClusterMonitoringRegistrationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "setClientType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct => mtricdManagementClusterMonitoringRegistrationStruct -> IO (Id NSNumber)
fabricIndex mtricdManagementClusterMonitoringRegistrationStruct  =
    sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRICDManagementClusterMonitoringRegistrationStruct mtricdManagementClusterMonitoringRegistrationStruct, IsNSNumber value) => mtricdManagementClusterMonitoringRegistrationStruct -> value -> IO ()
setFabricIndex mtricdManagementClusterMonitoringRegistrationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterMonitoringRegistrationStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkInNodeID@
checkInNodeIDSelector :: Selector
checkInNodeIDSelector = mkSelector "checkInNodeID"

-- | @Selector@ for @setCheckInNodeID:@
setCheckInNodeIDSelector :: Selector
setCheckInNodeIDSelector = mkSelector "setCheckInNodeID:"

-- | @Selector@ for @monitoredSubject@
monitoredSubjectSelector :: Selector
monitoredSubjectSelector = mkSelector "monitoredSubject"

-- | @Selector@ for @setMonitoredSubject:@
setMonitoredSubjectSelector :: Selector
setMonitoredSubjectSelector = mkSelector "setMonitoredSubject:"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @setClientType:@
setClientTypeSelector :: Selector
setClientTypeSelector = mkSelector "setClientType:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

