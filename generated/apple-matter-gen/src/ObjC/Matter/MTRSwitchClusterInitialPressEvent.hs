{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterInitialPressEvent@.
module ObjC.Matter.MTRSwitchClusterInitialPressEvent
  ( MTRSwitchClusterInitialPressEvent
  , IsMTRSwitchClusterInitialPressEvent(..)
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
newPosition :: IsMTRSwitchClusterInitialPressEvent mtrSwitchClusterInitialPressEvent => mtrSwitchClusterInitialPressEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterInitialPressEvent  =
    sendMsg mtrSwitchClusterInitialPressEvent (mkSelector "newPosition") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterInitialPressEvent mtrSwitchClusterInitialPressEvent, IsNSNumber value) => mtrSwitchClusterInitialPressEvent -> value -> IO ()
setNewPosition mtrSwitchClusterInitialPressEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterInitialPressEvent (mkSelector "setNewPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector
setNewPositionSelector = mkSelector "setNewPosition:"

