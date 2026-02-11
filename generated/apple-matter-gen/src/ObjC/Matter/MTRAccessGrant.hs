{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An access grant, which can be represented as an entry in the Matter Access Control cluster.
--
-- Generated bindings for @MTRAccessGrant@.
module ObjC.Matter.MTRAccessGrant
  ( MTRAccessGrant
  , IsMTRAccessGrant(..)
  , init_
  , new
  , accessGrantForNodeID_privilege
  , accessGrantForCASEAuthenticatedTag_privilege
  , accessGrantForGroupID_privilege
  , accessGrantForAllNodesWithPrivilege
  , subjectID
  , grantedPrivilege
  , authenticationMode
  , initSelector
  , newSelector
  , accessGrantForNodeID_privilegeSelector
  , accessGrantForCASEAuthenticatedTag_privilegeSelector
  , accessGrantForGroupID_privilegeSelector
  , accessGrantForAllNodesWithPrivilegeSelector
  , subjectIDSelector
  , grantedPrivilegeSelector
  , authenticationModeSelector

  -- * Enum types
  , MTRAccessControlEntryAuthMode(MTRAccessControlEntryAuthMode)
  , pattern MTRAccessControlEntryAuthModePASE
  , pattern MTRAccessControlEntryAuthModeCASE
  , pattern MTRAccessControlEntryAuthModeGroup
  , MTRAccessControlEntryPrivilege(MTRAccessControlEntryPrivilege)
  , pattern MTRAccessControlEntryPrivilegeView
  , pattern MTRAccessControlEntryPrivilegeProxyView
  , pattern MTRAccessControlEntryPrivilegeOperate
  , pattern MTRAccessControlEntryPrivilegeManage
  , pattern MTRAccessControlEntryPrivilegeAdminister

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO (Id MTRAccessGrant)
init_ mtrAccessGrant  =
    sendMsg mtrAccessGrant (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRAccessGrant)
new  =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Grant access at the provided level to a specific node on the fabric.  The provided nodeID must be an operational node identifier.
--
-- ObjC selector: @+ accessGrantForNodeID:privilege:@
accessGrantForNodeID_privilege :: IsNSNumber nodeID => nodeID -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForNodeID_privilege nodeID privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    withObjCPtr nodeID $ \raw_nodeID ->
      sendClassMsg cls' (mkSelector "accessGrantForNodeID:privilege:") (retPtr retVoid) [argPtr (castPtr raw_nodeID :: Ptr ()), argCUChar (coerce privilege)] >>= retainedObject . castPtr

-- | Grant access to any node on the fabric that has a matching CASE Authenticated Tag in its operational certificate.  The provided caseAuthenticatedTag must be a 32-bit unsigned integer with lower 16 bits not 0, per the Matter specification.
--
-- ObjC selector: @+ accessGrantForCASEAuthenticatedTag:privilege:@
accessGrantForCASEAuthenticatedTag_privilege :: IsNSNumber caseAuthenticatedTag => caseAuthenticatedTag -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForCASEAuthenticatedTag_privilege caseAuthenticatedTag privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    withObjCPtr caseAuthenticatedTag $ \raw_caseAuthenticatedTag ->
      sendClassMsg cls' (mkSelector "accessGrantForCASEAuthenticatedTag:privilege:") (retPtr retVoid) [argPtr (castPtr raw_caseAuthenticatedTag :: Ptr ()), argCUChar (coerce privilege)] >>= retainedObject . castPtr

-- | Grant access to any node on the fabric that is communicating with us via group messages sent to the given group.  The provided groupID must be a valid group identifier in the range 1-65535.
--
-- ObjC selector: @+ accessGrantForGroupID:privilege:@
accessGrantForGroupID_privilege :: IsNSNumber groupID => groupID -> MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForGroupID_privilege groupID privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    withObjCPtr groupID $ \raw_groupID ->
      sendClassMsg cls' (mkSelector "accessGrantForGroupID:privilege:") (retPtr retVoid) [argPtr (castPtr raw_groupID :: Ptr ()), argCUChar (coerce privilege)] >>= retainedObject . castPtr

-- | Grant access to any node on the fabric, as long as it's communicating with us over a unicast authenticated channel.
--
-- ObjC selector: @+ accessGrantForAllNodesWithPrivilege:@
accessGrantForAllNodesWithPrivilege :: MTRAccessControlEntryPrivilege -> IO (Id MTRAccessGrant)
accessGrantForAllNodesWithPrivilege privilege =
  do
    cls' <- getRequiredClass "MTRAccessGrant"
    sendClassMsg cls' (mkSelector "accessGrantForAllNodesWithPrivilege:") (retPtr retVoid) [argCUChar (coerce privilege)] >>= retainedObject . castPtr

-- | The matter access control subject ID that access has been granted for.  Nil when access has been granted for all subjects (e.g. via initForAllNodesWithPrivilege).
--
-- ObjC selector: @- subjectID@
subjectID :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO (Id NSNumber)
subjectID mtrAccessGrant  =
    sendMsg mtrAccessGrant (mkSelector "subjectID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The privilege that has been granted
--
-- ObjC selector: @- grantedPrivilege@
grantedPrivilege :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO MTRAccessControlEntryPrivilege
grantedPrivilege mtrAccessGrant  =
    fmap (coerce :: CUChar -> MTRAccessControlEntryPrivilege) $ sendMsg mtrAccessGrant (mkSelector "grantedPrivilege") retCUChar []

-- | The type of authentication mode the access grant is for. MTRAccessControlEntryAuthModeCASE for unicast messages and MTRAccessControlEntryAuthModeGroup for groupcast ones.
--
-- ObjC selector: @- authenticationMode@
authenticationMode :: IsMTRAccessGrant mtrAccessGrant => mtrAccessGrant -> IO MTRAccessControlEntryAuthMode
authenticationMode mtrAccessGrant  =
    fmap (coerce :: CUChar -> MTRAccessControlEntryAuthMode) $ sendMsg mtrAccessGrant (mkSelector "authenticationMode") retCUChar []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @accessGrantForNodeID:privilege:@
accessGrantForNodeID_privilegeSelector :: Selector
accessGrantForNodeID_privilegeSelector = mkSelector "accessGrantForNodeID:privilege:"

-- | @Selector@ for @accessGrantForCASEAuthenticatedTag:privilege:@
accessGrantForCASEAuthenticatedTag_privilegeSelector :: Selector
accessGrantForCASEAuthenticatedTag_privilegeSelector = mkSelector "accessGrantForCASEAuthenticatedTag:privilege:"

-- | @Selector@ for @accessGrantForGroupID:privilege:@
accessGrantForGroupID_privilegeSelector :: Selector
accessGrantForGroupID_privilegeSelector = mkSelector "accessGrantForGroupID:privilege:"

-- | @Selector@ for @accessGrantForAllNodesWithPrivilege:@
accessGrantForAllNodesWithPrivilegeSelector :: Selector
accessGrantForAllNodesWithPrivilegeSelector = mkSelector "accessGrantForAllNodesWithPrivilege:"

-- | @Selector@ for @subjectID@
subjectIDSelector :: Selector
subjectIDSelector = mkSelector "subjectID"

-- | @Selector@ for @grantedPrivilege@
grantedPrivilegeSelector :: Selector
grantedPrivilegeSelector = mkSelector "grantedPrivilege"

-- | @Selector@ for @authenticationMode@
authenticationModeSelector :: Selector
authenticationModeSelector = mkSelector "authenticationMode"

