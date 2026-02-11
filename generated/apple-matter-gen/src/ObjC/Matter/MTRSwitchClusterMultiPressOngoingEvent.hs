{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterMultiPressOngoingEvent@.
module ObjC.Matter.MTRSwitchClusterMultiPressOngoingEvent
  ( MTRSwitchClusterMultiPressOngoingEvent
  , IsMTRSwitchClusterMultiPressOngoingEvent(..)
  , newPosition
  , setNewPosition
  , currentNumberOfPressesCounted
  , setCurrentNumberOfPressesCounted
  , newPositionSelector
  , setNewPositionSelector
  , currentNumberOfPressesCountedSelector
  , setCurrentNumberOfPressesCountedSelector


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
newPosition :: IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent => mtrSwitchClusterMultiPressOngoingEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterMultiPressOngoingEvent  =
    sendMsg mtrSwitchClusterMultiPressOngoingEvent (mkSelector "newPosition") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent, IsNSNumber value) => mtrSwitchClusterMultiPressOngoingEvent -> value -> IO ()
setNewPosition mtrSwitchClusterMultiPressOngoingEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterMultiPressOngoingEvent (mkSelector "setNewPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentNumberOfPressesCounted@
currentNumberOfPressesCounted :: IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent => mtrSwitchClusterMultiPressOngoingEvent -> IO (Id NSNumber)
currentNumberOfPressesCounted mtrSwitchClusterMultiPressOngoingEvent  =
    sendMsg mtrSwitchClusterMultiPressOngoingEvent (mkSelector "currentNumberOfPressesCounted") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentNumberOfPressesCounted:@
setCurrentNumberOfPressesCounted :: (IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent, IsNSNumber value) => mtrSwitchClusterMultiPressOngoingEvent -> value -> IO ()
setCurrentNumberOfPressesCounted mtrSwitchClusterMultiPressOngoingEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterMultiPressOngoingEvent (mkSelector "setCurrentNumberOfPressesCounted:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector
setNewPositionSelector = mkSelector "setNewPosition:"

-- | @Selector@ for @currentNumberOfPressesCounted@
currentNumberOfPressesCountedSelector :: Selector
currentNumberOfPressesCountedSelector = mkSelector "currentNumberOfPressesCounted"

-- | @Selector@ for @setCurrentNumberOfPressesCounted:@
setCurrentNumberOfPressesCountedSelector :: Selector
setCurrentNumberOfPressesCountedSelector = mkSelector "setCurrentNumberOfPressesCounted:"

