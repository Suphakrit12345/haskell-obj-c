{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterMultiPressCompleteEvent@.
module ObjC.Matter.MTRSwitchClusterMultiPressCompleteEvent
  ( MTRSwitchClusterMultiPressCompleteEvent
  , IsMTRSwitchClusterMultiPressCompleteEvent(..)
  , previousPosition
  , setPreviousPosition
  , newPosition
  , setNewPosition
  , totalNumberOfPressesCounted
  , setTotalNumberOfPressesCounted
  , previousPositionSelector
  , setPreviousPositionSelector
  , newPositionSelector
  , setNewPositionSelector
  , totalNumberOfPressesCountedSelector
  , setTotalNumberOfPressesCountedSelector


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
previousPosition :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
previousPosition mtrSwitchClusterMultiPressCompleteEvent  =
    sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "previousPosition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousPosition:@
setPreviousPosition :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setPreviousPosition mtrSwitchClusterMultiPressCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "setPreviousPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newPosition@
newPosition :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterMultiPressCompleteEvent  =
    sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "newPosition") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setNewPosition mtrSwitchClusterMultiPressCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "setNewPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalNumberOfPressesCounted@
totalNumberOfPressesCounted :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
totalNumberOfPressesCounted mtrSwitchClusterMultiPressCompleteEvent  =
    sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "totalNumberOfPressesCounted") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalNumberOfPressesCounted:@
setTotalNumberOfPressesCounted :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setTotalNumberOfPressesCounted mtrSwitchClusterMultiPressCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSwitchClusterMultiPressCompleteEvent (mkSelector "setTotalNumberOfPressesCounted:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPosition@
previousPositionSelector :: Selector
previousPositionSelector = mkSelector "previousPosition"

-- | @Selector@ for @setPreviousPosition:@
setPreviousPositionSelector :: Selector
setPreviousPositionSelector = mkSelector "setPreviousPosition:"

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector
setNewPositionSelector = mkSelector "setNewPosition:"

-- | @Selector@ for @totalNumberOfPressesCounted@
totalNumberOfPressesCountedSelector :: Selector
totalNumberOfPressesCountedSelector = mkSelector "totalNumberOfPressesCounted"

-- | @Selector@ for @setTotalNumberOfPressesCounted:@
setTotalNumberOfPressesCountedSelector :: Selector
setTotalNumberOfPressesCountedSelector = mkSelector "setTotalNumberOfPressesCounted:"

