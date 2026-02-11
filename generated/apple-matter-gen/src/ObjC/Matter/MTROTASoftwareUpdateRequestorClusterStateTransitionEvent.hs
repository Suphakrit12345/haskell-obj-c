{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateRequestorClusterStateTransitionEvent@.
module ObjC.Matter.MTROtaSoftwareUpdateRequestorClusterStateTransitionEvent
  ( MTROtaSoftwareUpdateRequestorClusterStateTransitionEvent
  , IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent(..)
  , previousState
  , setPreviousState
  , newState
  , setNewState
  , reason
  , setReason
  , targetSoftwareVersion
  , setTargetSoftwareVersion
  , previousStateSelector
  , setPreviousStateSelector
  , newStateSelector
  , setNewStateSelector
  , reasonSelector
  , setReasonSelector
  , targetSoftwareVersionSelector
  , setTargetSoftwareVersionSelector


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

-- | @- previousState@
previousState :: IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
previousState mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "previousState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousState:@
setPreviousState :: (IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setPreviousState mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "setPreviousState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newState@
newState :: IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
newState mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "newState") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewState:@
setNewState :: (IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setNewState mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "setNewState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reason@
reason :: IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
reason mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReason:@
setReason :: (IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setReason mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "setReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetSoftwareVersion@
targetSoftwareVersion :: IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
targetSoftwareVersion mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "targetSoftwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetSoftwareVersion:@
setTargetSoftwareVersion :: (IsMTROtaSoftwareUpdateRequestorClusterStateTransitionEvent mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setTargetSoftwareVersion mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterStateTransitionEvent (mkSelector "setTargetSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousState@
previousStateSelector :: Selector
previousStateSelector = mkSelector "previousState"

-- | @Selector@ for @setPreviousState:@
setPreviousStateSelector :: Selector
setPreviousStateSelector = mkSelector "setPreviousState:"

-- | @Selector@ for @newState@
newStateSelector :: Selector
newStateSelector = mkSelector "newState"

-- | @Selector@ for @setNewState:@
setNewStateSelector :: Selector
setNewStateSelector = mkSelector "setNewState:"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector
setReasonSelector = mkSelector "setReason:"

-- | @Selector@ for @targetSoftwareVersion@
targetSoftwareVersionSelector :: Selector
targetSoftwareVersionSelector = mkSelector "targetSoftwareVersion"

-- | @Selector@ for @setTargetSoftwareVersion:@
setTargetSoftwareVersionSelector :: Selector
setTargetSoftwareVersionSelector = mkSelector "setTargetSoftwareVersion:"

