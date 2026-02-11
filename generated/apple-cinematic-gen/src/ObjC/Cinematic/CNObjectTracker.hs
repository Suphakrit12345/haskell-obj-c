{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Converts a normalized point or rectangle into a detection track that tracks an object over time.
--
-- Generated bindings for @CNObjectTracker@.
module ObjC.Cinematic.CNObjectTracker
  ( CNObjectTracker
  , IsCNObjectTracker(..)
  , initWithCommandQueue
  , finishDetectionTrack
  , resetDetectionTrack
  , init_
  , new
  , isSupported
  , initWithCommandQueueSelector
  , finishDetectionTrackSelector
  , resetDetectionTrackSelector
  , initSelector
  , newSelector
  , isSupportedSelector


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

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new detection track builder. - Parameters:   - commandQueue: the command queue of a metal device to which commands should be submitted to perform work
--
-- ObjC selector: @- initWithCommandQueue:@
initWithCommandQueue :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> RawId -> IO (Id CNObjectTracker)
initWithCommandQueue cnObjectTracker  commandQueue =
    sendMsg cnObjectTracker (mkSelector "initWithCommandQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ())] >>= ownedObject . castPtr

-- | Finish constructing the detection track and return it. - Returns: a detection track which tracks the object
--
-- ObjC selector: @- finishDetectionTrack@
finishDetectionTrack :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO (Id CNDetectionTrack)
finishDetectionTrack cnObjectTracker  =
    sendMsg cnObjectTracker (mkSelector "finishDetectionTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reset the builder to construct a new detection track.
--
-- ObjC selector: @- resetDetectionTrack@
resetDetectionTrack :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO ()
resetDetectionTrack cnObjectTracker  =
    sendMsg cnObjectTracker (mkSelector "resetDetectionTrack") retVoid []

-- | @- init@
init_ :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO (Id CNObjectTracker)
init_ cnObjectTracker  =
    sendMsg cnObjectTracker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNObjectTracker)
new  =
  do
    cls' <- getRequiredClass "CNObjectTracker"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates whether the current device supports object detection and tracking.
--
-- ObjC selector: @+ isSupported@
isSupported :: IO Bool
isSupported  =
  do
    cls' <- getRequiredClass "CNObjectTracker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isSupported") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCommandQueue:@
initWithCommandQueueSelector :: Selector
initWithCommandQueueSelector = mkSelector "initWithCommandQueue:"

-- | @Selector@ for @finishDetectionTrack@
finishDetectionTrackSelector :: Selector
finishDetectionTrackSelector = mkSelector "finishDetectionTrack"

-- | @Selector@ for @resetDetectionTrack@
resetDetectionTrackSelector :: Selector
resetDetectionTrackSelector = mkSelector "resetDetectionTrack"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector
isSupportedSelector = mkSelector "isSupported"

