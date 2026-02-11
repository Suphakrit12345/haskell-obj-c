{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct(..)
  , privilege
  , setPrivilege
  , authMode
  , setAuthMode
  , subjects
  , setSubjects
  , targets
  , setTargets
  , privilegeSelector
  , setPrivilegeSelector
  , authModeSelector
  , setAuthModeSelector
  , subjectsSelector
  , setSubjectsSelector
  , targetsSelector
  , setTargetsSelector


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

-- | @- privilege@
privilege :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSNumber)
privilege mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "privilege") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrivilege:@
setPrivilege :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setPrivilege mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "setPrivilege:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authMode@
authMode :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSNumber)
authMode mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "authMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthMode:@
setAuthMode :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setAuthMode mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "setAuthMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subjects@
subjects :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSArray)
subjects mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "subjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubjects:@
setSubjects :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSArray value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setSubjects mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "setSubjects:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targets@
targets :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> IO (Id NSArray)
targets mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "targets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargets:@
setTargets :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct, IsNSArray value) => mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct -> value -> IO ()
setTargets mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAccessControlEntryStruct (mkSelector "setTargets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @privilege@
privilegeSelector :: Selector
privilegeSelector = mkSelector "privilege"

-- | @Selector@ for @setPrivilege:@
setPrivilegeSelector :: Selector
setPrivilegeSelector = mkSelector "setPrivilege:"

-- | @Selector@ for @authMode@
authModeSelector :: Selector
authModeSelector = mkSelector "authMode"

-- | @Selector@ for @setAuthMode:@
setAuthModeSelector :: Selector
setAuthModeSelector = mkSelector "setAuthMode:"

-- | @Selector@ for @subjects@
subjectsSelector :: Selector
subjectsSelector = mkSelector "subjects"

-- | @Selector@ for @setSubjects:@
setSubjectsSelector :: Selector
setSubjectsSelector = mkSelector "setSubjects:"

-- | @Selector@ for @targets@
targetsSelector :: Selector
targetsSelector = mkSelector "targets"

-- | @Selector@ for @setTargets:@
setTargetsSelector :: Selector
setTargetsSelector = mkSelector "setTargets:"

