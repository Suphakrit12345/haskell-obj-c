{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOverallCurrentStateStruct@.
module ObjC.Matter.MTRClosureControlClusterOverallCurrentStateStruct
  ( MTRClosureControlClusterOverallCurrentStateStruct
  , IsMTRClosureControlClusterOverallCurrentStateStruct(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , secureState
  , setSecureState
  , positionSelector
  , setPositionSelector
  , latchSelector
  , setLatchSelector
  , speedSelector
  , setSpeedSelector
  , secureStateSelector
  , setSecureStateSelector


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
position :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
position mtrClosureControlClusterOverallCurrentStateStruct  =
    sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setPosition mtrClosureControlClusterOverallCurrentStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latch@
latch :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
latch mtrClosureControlClusterOverallCurrentStateStruct  =
    sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "latch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatch:@
setLatch :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setLatch mtrClosureControlClusterOverallCurrentStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "setLatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
speed mtrClosureControlClusterOverallCurrentStateStruct  =
    sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setSpeed mtrClosureControlClusterOverallCurrentStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- secureState@
secureState :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
secureState mtrClosureControlClusterOverallCurrentStateStruct  =
    sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "secureState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecureState:@
setSecureState :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setSecureState mtrClosureControlClusterOverallCurrentStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOverallCurrentStateStruct (mkSelector "setSecureState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @secureState@
secureStateSelector :: Selector
secureStateSelector = mkSelector "secureState"

-- | @Selector@ for @setSecureState:@
setSecureStateSelector :: Selector
setSecureStateSelector = mkSelector "setSecureState:"

