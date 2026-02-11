{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetUpdatedEvent@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetUpdatedEvent
  ( MTRTargetNavigatorClusterTargetUpdatedEvent
  , IsMTRTargetNavigatorClusterTargetUpdatedEvent(..)
  , targetList
  , setTargetList
  , currentTarget
  , setCurrentTarget
  , data_
  , setData
  , targetListSelector
  , setTargetListSelector
  , currentTargetSelector
  , setCurrentTargetSelector
  , dataSelector
  , setDataSelector


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

-- | @- targetList@
targetList :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSArray)
targetList mtrTargetNavigatorClusterTargetUpdatedEvent  =
    sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "targetList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetList:@
setTargetList :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSArray value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setTargetList mtrTargetNavigatorClusterTargetUpdatedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "setTargetList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentTarget@
currentTarget :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSNumber)
currentTarget mtrTargetNavigatorClusterTargetUpdatedEvent  =
    sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "currentTarget") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentTarget:@
setCurrentTarget :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSNumber value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setCurrentTarget mtrTargetNavigatorClusterTargetUpdatedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "setCurrentTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSData)
data_ mtrTargetNavigatorClusterTargetUpdatedEvent  =
    sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSData value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setData mtrTargetNavigatorClusterTargetUpdatedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTargetNavigatorClusterTargetUpdatedEvent (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetList@
targetListSelector :: Selector
targetListSelector = mkSelector "targetList"

-- | @Selector@ for @setTargetList:@
setTargetListSelector :: Selector
setTargetListSelector = mkSelector "setTargetList:"

-- | @Selector@ for @currentTarget@
currentTargetSelector :: Selector
currentTargetSelector = mkSelector "currentTarget"

-- | @Selector@ for @setCurrentTarget:@
setCurrentTargetSelector :: Selector
setCurrentTargetSelector = mkSelector "setCurrentTarget:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

