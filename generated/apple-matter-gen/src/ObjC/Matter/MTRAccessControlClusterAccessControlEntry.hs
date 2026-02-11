{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntry@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntry
  ( MTRAccessControlClusterAccessControlEntry
  , IsMTRAccessControlClusterAccessControlEntry(..)
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
privilege :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
privilege mtrAccessControlClusterAccessControlEntry  =
    sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "privilege") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrivilege:@
setPrivilege :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setPrivilege mtrAccessControlClusterAccessControlEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "setPrivilege:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authMode@
authMode :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
authMode mtrAccessControlClusterAccessControlEntry  =
    sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "authMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthMode:@
setAuthMode :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setAuthMode mtrAccessControlClusterAccessControlEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "setAuthMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subjects@
subjects :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSArray)
subjects mtrAccessControlClusterAccessControlEntry  =
    sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "subjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubjects:@
setSubjects :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSArray value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setSubjects mtrAccessControlClusterAccessControlEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "setSubjects:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targets@
targets :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSArray)
targets mtrAccessControlClusterAccessControlEntry  =
    sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "targets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargets:@
setTargets :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSArray value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setTargets mtrAccessControlClusterAccessControlEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "setTargets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry => mtrAccessControlClusterAccessControlEntry -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntry  =
    sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntry mtrAccessControlClusterAccessControlEntry, IsNSNumber value) => mtrAccessControlClusterAccessControlEntry -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntry (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

