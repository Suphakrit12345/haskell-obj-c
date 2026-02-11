{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterDimensionStateStruct@.
module ObjC.Matter.MTRClosureDimensionClusterDimensionStateStruct
  ( MTRClosureDimensionClusterDimensionStateStruct
  , IsMTRClosureDimensionClusterDimensionStateStruct(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , positionSelector
  , setPositionSelector
  , latchSelector
  , setLatchSelector
  , speedSelector
  , setSpeedSelector


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

-- | @- position@
position :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
position mtrClosureDimensionClusterDimensionStateStruct  =
    sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setPosition mtrClosureDimensionClusterDimensionStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latch@
latch :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
latch mtrClosureDimensionClusterDimensionStateStruct  =
    sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "latch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatch:@
setLatch :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setLatch mtrClosureDimensionClusterDimensionStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "setLatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
speed mtrClosureDimensionClusterDimensionStateStruct  =
    sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setSpeed mtrClosureDimensionClusterDimensionStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterDimensionStateStruct (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @latch@
latchSelector :: Selector
latchSelector = mkSelector "latch"

-- | @Selector@ for @setLatch:@
setLatchSelector :: Selector
setLatchSelector = mkSelector "setLatch:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

