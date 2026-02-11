{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAsyncCallbackQueueWorkItem@.
module ObjC.Matter.MTRAsyncCallbackQueueWorkItem
  ( MTRAsyncCallbackQueueWorkItem
  , IsMTRAsyncCallbackQueueWorkItem(..)
  , init_
  , new
  , initWithQueue
  , endWork
  , retryWork
  , readyHandler
  , setReadyHandler
  , cancelHandler
  , setCancelHandler
  , initSelector
  , newSelector
  , initWithQueueSelector
  , endWorkSelector
  , retryWorkSelector
  , readyHandlerSelector
  , setReadyHandlerSelector
  , cancelHandlerSelector
  , setCancelHandlerSelector


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
init_ :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Id MTRAsyncCallbackQueueWorkItem)
init_ mtrAsyncCallbackQueueWorkItem  =
    sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRAsyncCallbackQueueWorkItem)
new  =
  do
    cls' <- getRequiredClass "MTRAsyncCallbackQueueWorkItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithQueue:@
initWithQueue :: (IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem, IsNSObject queue) => mtrAsyncCallbackQueueWorkItem -> queue -> IO (Id MTRAsyncCallbackQueueWorkItem)
initWithQueue mtrAsyncCallbackQueueWorkItem  queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "initWithQueue:") (retPtr retVoid) [argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- endWork@
endWork :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO ()
endWork mtrAsyncCallbackQueueWorkItem  =
    sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "endWork") retVoid []

-- | @- retryWork@
retryWork :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO ()
retryWork mtrAsyncCallbackQueueWorkItem  =
    sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "retryWork") retVoid []

-- | @- readyHandler@
readyHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Ptr ())
readyHandler mtrAsyncCallbackQueueWorkItem  =
    fmap castPtr $ sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "readyHandler") (retPtr retVoid) []

-- | @- setReadyHandler:@
setReadyHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> Ptr () -> IO ()
setReadyHandler mtrAsyncCallbackQueueWorkItem  value =
    sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "setReadyHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- cancelHandler@
cancelHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Ptr ())
cancelHandler mtrAsyncCallbackQueueWorkItem  =
    fmap castPtr $ sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "cancelHandler") (retPtr retVoid) []

-- | @- setCancelHandler:@
setCancelHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> Ptr () -> IO ()
setCancelHandler mtrAsyncCallbackQueueWorkItem  value =
    sendMsg mtrAsyncCallbackQueueWorkItem (mkSelector "setCancelHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithQueue:@
initWithQueueSelector :: Selector
initWithQueueSelector = mkSelector "initWithQueue:"

-- | @Selector@ for @endWork@
endWorkSelector :: Selector
endWorkSelector = mkSelector "endWork"

-- | @Selector@ for @retryWork@
retryWorkSelector :: Selector
retryWorkSelector = mkSelector "retryWork"

-- | @Selector@ for @readyHandler@
readyHandlerSelector :: Selector
readyHandlerSelector = mkSelector "readyHandler"

-- | @Selector@ for @setReadyHandler:@
setReadyHandlerSelector :: Selector
setReadyHandlerSelector = mkSelector "setReadyHandler:"

-- | @Selector@ for @cancelHandler@
cancelHandlerSelector :: Selector
cancelHandlerSelector = mkSelector "cancelHandler"

-- | @Selector@ for @setCancelHandler:@
setCancelHandlerSelector :: Selector
setCancelHandlerSelector = mkSelector "setCancelHandler:"

