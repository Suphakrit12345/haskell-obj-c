{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntryStruct
  ( MTRAccessControlClusterAccessControlEntryStruct
  , IsMTRAccessControlClusterAccessControlEntryStruct(..)
  , privilege
  , setPrivilege
  , authMode
  , setAuthMode
  , subjects
  , setSubjects
  , targets
  , setTargets
  , fabricIndex
  , setFabricIndex
  , privilegeSelector
  , setPrivilegeSelector
  , authModeSelector
  , setAuthModeSelector
  , subjectsSelector
  , setSubjectsSelector
  , targetsSelector
  , setTargetsSelector
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

-- | @- privilege@
privilege :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
privilege mtrAccessControlClusterAccessControlEntryStruct  =
    sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "privilege") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrivilege:@
setPrivilege :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setPrivilege mtrAccessControlClusterAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "setPrivilege:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authMode@
authMode :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
authMode mtrAccessControlClusterAccessControlEntryStruct  =
    sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "authMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthMode:@
setAuthMode :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setAuthMode mtrAccessControlClusterAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "setAuthMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subjects@
subjects :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSArray)
subjects mtrAccessControlClusterAccessControlEntryStruct  =
    sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "subjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubjects:@
setSubjects :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setSubjects mtrAccessControlClusterAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "setSubjects:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targets@
targets :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSArray)
targets mtrAccessControlClusterAccessControlEntryStruct  =
    sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "targets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargets:@
setTargets :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setTargets mtrAccessControlClusterAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "setTargets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct => mtrAccessControlClusterAccessControlEntryStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntryStruct  =
    sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntryStruct mtrAccessControlClusterAccessControlEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

