{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterActionFailedEvent@.
module ObjC.Matter.MTRActionsClusterActionFailedEvent
  ( MTRActionsClusterActionFailedEvent
  , IsMTRActionsClusterActionFailedEvent(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , newState
  , setNewState
  , error_
  , setError
  , actionIDSelector
  , setActionIDSelector
  , invokeIDSelector
  , setInvokeIDSelector
  , newStateSelector
  , setNewStateSelector
  , errorSelector
  , setErrorSelector


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
actionID :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
actionID mtrActionsClusterActionFailedEvent  =
    sendMsg mtrActionsClusterActionFailedEvent (mkSelector "actionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setActionID mtrActionsClusterActionFailedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionFailedEvent (mkSelector "setActionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- invokeID@
invokeID :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
invokeID mtrActionsClusterActionFailedEvent  =
    sendMsg mtrActionsClusterActionFailedEvent (mkSelector "invokeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setInvokeID mtrActionsClusterActionFailedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionFailedEvent (mkSelector "setInvokeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newState@
newState :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
newState mtrActionsClusterActionFailedEvent  =
    sendMsg mtrActionsClusterActionFailedEvent (mkSelector "newState") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewState:@
setNewState :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setNewState mtrActionsClusterActionFailedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionFailedEvent (mkSelector "setNewState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- error@
error_ :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
error_ mtrActionsClusterActionFailedEvent  =
    sendMsg mtrActionsClusterActionFailedEvent (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setError:@
setError :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setError mtrActionsClusterActionFailedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionFailedEvent (mkSelector "setError:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @setError:@
setErrorSelector :: Selector
setErrorSelector = mkSelector "setError:"

