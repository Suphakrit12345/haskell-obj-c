{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterShortReleaseEvent@.
module ObjC.Matter.MTRSwitchClusterShortReleaseEvent
  ( MTRSwitchClusterShortReleaseEvent
  , IsMTRSwitchClusterShortReleaseEvent(..)
  , previousPosition
  , setPreviousPosition
  , previousPositionSelector
  , setPreviousPositionSelector


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

-- | @- previousPosition@
previousPosition :: IsMTRSwitchClusterShortReleaseEvent mtrSwitchClusterShortReleaseEvent => mtrSwitchClusterShortReleaseEvent -> IO (Id NSNumber)
previousPosition mtrSwitchClusterShortReleaseEvent  =
    sendMsg mtrSwitchClusterShortReleaseEvent (mkSelector "previousPosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousPosition:@
setPreviousPosition :: (IsMTRSwitchClusterShortReleaseEvent mtrSwitchClusterShortReleaseEvent, IsNSNumber value) => mtrSwitchClusterShortReleaseEvent -> value -> IO ()
setPreviousPosition mtrSwitchClusterShortReleaseEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterShortReleaseEvent (mkSelector "setPreviousPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPosition@
previousPositionSelector :: Selector
previousPositionSelector = mkSelector "previousPosition"

-- | @Selector@ for @setPreviousPosition:@
setPreviousPositionSelector :: Selector
setPreviousPositionSelector = mkSelector "setPreviousPosition:"

