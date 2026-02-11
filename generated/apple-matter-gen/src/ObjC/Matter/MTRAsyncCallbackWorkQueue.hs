{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAsyncCallbackWorkQueue@.
module ObjC.Matter.MTRAsyncCallbackWorkQueue
  ( MTRAsyncCallbackWorkQueue
  , IsMTRAsyncCallbackWorkQueue(..)
  , init_
  , new
  , initWithContext_queue
  , invalidate
  , enqueueWorkItem
  , initSelector
  , newSelector
  , initWithContext_queueSelector
  , invalidateSelector
  , enqueueWorkItemSelector


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

-- | @- init@
init_ :: IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue => mtrAsyncCallbackWorkQueue -> IO (Id MTRAsyncCallbackWorkQueue)
init_ mtrAsyncCallbackWorkQueue  =
    sendMsg mtrAsyncCallbackWorkQueue (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRAsyncCallbackWorkQueue)
new  =
  do
    cls' <- getRequiredClass "MTRAsyncCallbackWorkQueue"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithContext:queue:@
initWithContext_queue :: (IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue, IsNSObject queue) => mtrAsyncCallbackWorkQueue -> RawId -> queue -> IO (Id MTRAsyncCallbackWorkQueue)
initWithContext_queue mtrAsyncCallbackWorkQueue  context queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrAsyncCallbackWorkQueue (mkSelector "initWithContext:queue:") (retPtr retVoid) [argPtr (castPtr (unRawId context) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- invalidate@
invalidate :: IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue => mtrAsyncCallbackWorkQueue -> IO ()
invalidate mtrAsyncCallbackWorkQueue  =
    sendMsg mtrAsyncCallbackWorkQueue (mkSelector "invalidate") retVoid []

-- | @- enqueueWorkItem:@
enqueueWorkItem :: (IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue, IsMTRAsyncCallbackQueueWorkItem item) => mtrAsyncCallbackWorkQueue -> item -> IO ()
enqueueWorkItem mtrAsyncCallbackWorkQueue  item =
  withObjCPtr item $ \raw_item ->
      sendMsg mtrAsyncCallbackWorkQueue (mkSelector "enqueueWorkItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithContext:queue:@
initWithContext_queueSelector :: Selector
initWithContext_queueSelector = mkSelector "initWithContext:queue:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @enqueueWorkItem:@
enqueueWorkItemSelector :: Selector
enqueueWorkItemSelector = mkSelector "enqueueWorkItem:"

