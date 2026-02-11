{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableComposition@.
module ObjC.Cinematic.AVMutableComposition
  ( AVMutableComposition
  , IsAVMutableComposition(..)
  , addTracksForCinematicAssetInfo_preferredStartingTrackID
  , addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector


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
import ObjC.AVFoundation.Internal.Classes

-- | Adds a group of empty tracks associated with a cinematic asset to a mutable composition. - Returns: Information about the composition tracks added to the mutable composition. Be sure to call insertTimeRange on the result to specify at least one time range of cinematic asset you'd like in the composition.
--
-- ObjC selector: @- addTracksForCinematicAssetInfo:preferredStartingTrackID:@
addTracksForCinematicAssetInfo_preferredStartingTrackID :: (IsAVMutableComposition avMutableComposition, IsCNAssetInfo assetInfo) => avMutableComposition -> assetInfo -> CInt -> IO (Id CNCompositionInfo)
addTracksForCinematicAssetInfo_preferredStartingTrackID avMutableComposition  assetInfo preferredStartingTrackID =
  withObjCPtr assetInfo $ \raw_assetInfo ->
      sendMsg avMutableComposition (mkSelector "addTracksForCinematicAssetInfo:preferredStartingTrackID:") (retPtr retVoid) [argPtr (castPtr raw_assetInfo :: Ptr ()), argCInt preferredStartingTrackID] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTracksForCinematicAssetInfo:preferredStartingTrackID:@
addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector :: Selector
addTracksForCinematicAssetInfo_preferredStartingTrackIDSelector = mkSelector "addTracksForCinematicAssetInfo:preferredStartingTrackID:"

