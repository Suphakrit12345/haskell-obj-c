{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterStateChangedEvent@.
module ObjC.Matter.MTRActionsClusterStateChangedEvent
  ( MTRActionsClusterStateChangedEvent
  , IsMTRActionsClusterStateChangedEvent(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , newState
  , setNewState
  , actionIDSelector
  , setActionIDSelector
  , invokeIDSelector
  , setInvokeIDSelector
  , newStateSelector
  , setNewStateSelector


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

-- | @- actionID@
actionID :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
actionID mtrActionsClusterStateChangedEvent  =
    sendMsg mtrActionsClusterStateChangedEvent (mkSelector "actionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setActionID mtrActionsClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStateChangedEvent (mkSelector "setActionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- invokeID@
invokeID :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
invokeID mtrActionsClusterStateChangedEvent  =
    sendMsg mtrActionsClusterStateChangedEvent (mkSelector "invokeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setInvokeID mtrActionsClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStateChangedEvent (mkSelector "setInvokeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newState@
newState :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
newState mtrActionsClusterStateChangedEvent  =
    sendMsg mtrActionsClusterStateChangedEvent (mkSelector "newState") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewState:@
setNewState :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setNewState mtrActionsClusterStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStateChangedEvent (mkSelector "setNewState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector
setActionIDSelector = mkSelector "setActionID:"

-- | @Selector@ for @invokeID@
invokeIDSelector :: Selector
invokeIDSelector = mkSelector "invokeID"

-- | @Selector@ for @setInvokeID:@
setInvokeIDSelector :: Selector
setInvokeIDSelector = mkSelector "setInvokeID:"

-- | @Selector@ for @newState@
newStateSelector :: Selector
newStateSelector = mkSelector "newState"

-- | @Selector@ for @setNewState:@
setNewStateSelector :: Selector
setNewStateSelector = mkSelector "setNewState:"

