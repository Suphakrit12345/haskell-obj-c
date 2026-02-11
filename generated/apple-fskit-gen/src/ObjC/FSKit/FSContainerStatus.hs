{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that represents a container's status.
--
-- This type contains two properties:
--
-- * The ``state`` value that indicates the state of the container, such as ``FSContainerState/ready`` or ``FSContainerState/blocked``. * The ``status`` is an error (optional in Swift, nullable in Objective-C) that provides further information about the state, such as why the container is blocked.
--
-- Examples of statuses that require intervention include errors that indicate the container isn't ready (POSIX @EAGAIN@ or @ENOTCONN@), the container needs authentication (@ENEEDAUTH@), or that authentication failed (@EAUTH@). The status can also be an informative error, such as the FSKit error ``FSError/Code/statusOperationInProgress``.
--
-- Generated bindings for @FSContainerStatus@.
module ObjC.FSKit.FSContainerStatus
  ( FSContainerStatus
  , IsFSContainerStatus(..)
  , init_
  , activeWithStatus
  , blockedWithStatus
  , notReadyWithStatus
  , readyWithStatus
  , state
  , status
  , active
  , ready
  , initSelector
  , activeWithStatusSelector
  , blockedWithStatusSelector
  , notReadyWithStatusSelector
  , readyWithStatusSelector
  , stateSelector
  , statusSelector
  , activeSelector
  , readySelector

  -- * Enum types
  , FSContainerState(FSContainerState)
  , pattern FSContainerStateNotReady
  , pattern FSContainerStateBlocked
  , pattern FSContainerStateReady
  , pattern FSContainerStateActive

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

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO (Id FSContainerStatus)
init_ fsContainerStatus  =
    sendMsg fsContainerStatus (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns a active container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ activeWithStatus:@
activeWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
activeWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    withObjCPtr errorStatus $ \raw_errorStatus ->
      sendClassMsg cls' (mkSelector "activeWithStatus:") (retPtr retVoid) [argPtr (castPtr raw_errorStatus :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a blocked container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ blockedWithStatus:@
blockedWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
blockedWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    withObjCPtr errorStatus $ \raw_errorStatus ->
      sendClassMsg cls' (mkSelector "blockedWithStatus:") (retPtr retVoid) [argPtr (castPtr raw_errorStatus :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a not-ready container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ notReadyWithStatus:@
notReadyWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
notReadyWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    withObjCPtr errorStatus $ \raw_errorStatus ->
      sendClassMsg cls' (mkSelector "notReadyWithStatus:") (retPtr retVoid) [argPtr (castPtr raw_errorStatus :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a ready container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ readyWithStatus:@
readyWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
readyWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    withObjCPtr errorStatus $ \raw_errorStatus ->
      sendClassMsg cls' (mkSelector "readyWithStatus:") (retPtr retVoid) [argPtr (castPtr raw_errorStatus :: Ptr ())] >>= retainedObject . castPtr

-- | A value that represents the container state, such as ready, active, or blocked.
--
-- ObjC selector: @- state@
state :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO FSContainerState
state fsContainerStatus  =
    fmap (coerce :: CLong -> FSContainerState) $ sendMsg fsContainerStatus (mkSelector "state") retCLong []

-- | An optional error that provides further information about the state.
--
-- ObjC selector: @- status@
status :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO (Id NSError)
status fsContainerStatus  =
    sendMsg fsContainerStatus (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A status that represents an active container with no error.
--
-- This value is a ``FSContainerStatus`` with a ``state`` that is ``active``, and has a ``status`` that is @nil@.
--
-- ObjC selector: @+ active@
active :: IO (Id FSContainerStatus)
active  =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMsg cls' (mkSelector "active") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A status that represents a ready container with no error.
--
-- This value is a ``FSContainerStatus`` with a ``state`` that is ``ready``, and a ``status`` that is @nil@.
--
-- ObjC selector: @+ ready@
ready :: IO (Id FSContainerStatus)
ready  =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMsg cls' (mkSelector "ready") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @activeWithStatus:@
activeWithStatusSelector :: Selector
activeWithStatusSelector = mkSelector "activeWithStatus:"

-- | @Selector@ for @blockedWithStatus:@
blockedWithStatusSelector :: Selector
blockedWithStatusSelector = mkSelector "blockedWithStatus:"

-- | @Selector@ for @notReadyWithStatus:@
notReadyWithStatusSelector :: Selector
notReadyWithStatusSelector = mkSelector "notReadyWithStatus:"

-- | @Selector@ for @readyWithStatus:@
readyWithStatusSelector :: Selector
readyWithStatusSelector = mkSelector "readyWithStatus:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @ready@
readySelector :: Selector
readySelector = mkSelector "ready"

