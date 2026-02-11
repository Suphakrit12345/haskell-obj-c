{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOverallTargetStateStruct@.
module ObjC.Matter.MTRClosureControlClusterOverallTargetStateStruct
  ( MTRClosureControlClusterOverallTargetStateStruct
  , IsMTRClosureControlClusterOverallTargetStateStruct(..)
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
position :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
position mtrClosureControlClusterOverallTargetStateStruct  =
    sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setPosition mtrClosureControlClusterOverallTargetStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latch@
latch :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
latch mtrClosureControlClusterOverallTargetStateStruct  =
    sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "latch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatch:@
setLatch :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setLatch mtrClosureControlClusterOverallTargetStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "setLatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
speed mtrClosureControlClusterOverallTargetStateStruct  =
    sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setSpeed mtrClosureControlClusterOverallTargetStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallTargetStateStruct (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

