{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAudioLevel@.
module ObjC.SensorKit.SRAudioLevel
  ( SRAudioLevel
  , IsSRAudioLevel(..)
  , init_
  , new
  , loudness
  , initSelector
  , newSelector
  , loudnessSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRAudioLevel srAudioLevel => srAudioLevel -> IO (Id SRAudioLevel)
init_ srAudioLevel  =
    sendMsg srAudioLevel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRAudioLevel)
new  =
  do
    cls' <- getRequiredClass "SRAudioLevel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | loudness
--
-- Measure of the audio level in decibels
--
-- ObjC selector: @- loudness@
loudness :: IsSRAudioLevel srAudioLevel => srAudioLevel -> IO CDouble
loudness srAudioLevel  =
    sendMsg srAudioLevel (mkSelector "loudness") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @loudness@
loudnessSelector :: Selector
loudnessSelector = mkSelector "loudness"

