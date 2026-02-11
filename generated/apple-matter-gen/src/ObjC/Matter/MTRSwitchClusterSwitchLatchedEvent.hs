{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterSwitchLatchedEvent@.
module ObjC.Matter.MTRSwitchClusterSwitchLatchedEvent
  ( MTRSwitchClusterSwitchLatchedEvent
  , IsMTRSwitchClusterSwitchLatchedEvent(..)
  , newPosition
  , setNewPosition
  , newPositionSelector
  , setNewPositionSelector


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

-- | @- newPosition@
newPosition :: IsMTRSwitchClusterSwitchLatchedEvent mtrSwitchClusterSwitchLatchedEvent => mtrSwitchClusterSwitchLatchedEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterSwitchLatchedEvent  =
    sendMsg mtrSwitchClusterSwitchLatchedEvent (mkSelector "newPosition") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterSwitchLatchedEvent mtrSwitchClusterSwitchLatchedEvent, IsNSNumber value) => mtrSwitchClusterSwitchLatchedEvent -> value -> IO ()
setNewPosition mtrSwitchClusterSwitchLatchedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterSwitchLatchedEvent (mkSelector "setNewPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector
setNewPositionSelector = mkSelector "setNewPosition:"

