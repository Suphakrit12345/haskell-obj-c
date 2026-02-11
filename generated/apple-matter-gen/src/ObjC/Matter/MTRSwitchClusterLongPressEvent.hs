{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterLongPressEvent@.
module ObjC.Matter.MTRSwitchClusterLongPressEvent
  ( MTRSwitchClusterLongPressEvent
  , IsMTRSwitchClusterLongPressEvent(..)
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
newPosition :: IsMTRSwitchClusterLongPressEvent mtrSwitchClusterLongPressEvent => mtrSwitchClusterLongPressEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterLongPressEvent  =
    sendMsg mtrSwitchClusterLongPressEvent (mkSelector "newPosition") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterLongPressEvent mtrSwitchClusterLongPressEvent, IsNSNumber value) => mtrSwitchClusterLongPressEvent -> value -> IO ()
setNewPosition mtrSwitchClusterLongPressEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterLongPressEvent (mkSelector "setNewPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector
setNewPositionSelector = mkSelector "setNewPosition:"

