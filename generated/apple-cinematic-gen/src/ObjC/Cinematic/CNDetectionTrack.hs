{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Abstract class representing a series of detections of the same subject over time.
--
-- Generated bindings for @CNDetectionTrack@.
module ObjC.Cinematic.CNDetectionTrack
  ( CNDetectionTrack
  , IsCNDetectionTrack(..)
  , init_
  , new
  , detectionType
  , detectionID
  , detectionGroupID
  , userCreated
  , discrete
  , initSelector
  , newSelector
  , detectionTypeSelector
  , detectionIDSelector
  , detectionGroupIDSelector
  , userCreatedSelector
  , discreteSelector

  -- * Enum types
  , CNDetectionType(CNDetectionType)
  , pattern CNDetectionTypeUnknown
  , pattern CNDetectionTypeHumanFace
  , pattern CNDetectionTypeHumanHead
  , pattern CNDetectionTypeHumanTorso
  , pattern CNDetectionTypeCatBody
  , pattern CNDetectionTypeDogBody
  , pattern CNDetectionTypeCatHead
  , pattern CNDetectionTypeDogHead
  , pattern CNDetectionTypeSportsBall
  , pattern CNDetectionTypeAutoFocus
  , pattern CNDetectionTypeFixedFocus
  , pattern CNDetectionTypeCustom

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
import ObjC.Cinematic.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO (Id CNDetectionTrack)
init_ cnDetectionTrack  =
    sendMsg cnDetectionTrack (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNDetectionTrack)
new  =
  do
    cls' <- getRequiredClass "CNDetectionTrack"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The type of subject detected by this detection track.
--
-- ObjC selector: @- detectionType@
detectionType :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CNDetectionType
detectionType cnDetectionTrack  =
    fmap (coerce :: CLong -> CNDetectionType) $ sendMsg cnDetectionTrack (mkSelector "detectionType") retCLong []

-- | The detectionID of the subject detected during this track; unique within a cinematic script.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CLong
detectionID cnDetectionTrack  =
    sendMsg cnDetectionTrack (mkSelector "detectionID") retCLong []

-- | The detectionGroupID of the subject detected by the track.
--
-- The detectionGroupID can be used to associate related detections such as the face and torso of the same person.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CLong
detectionGroupID cnDetectionTrack  =
    sendMsg cnDetectionTrack (mkSelector "detectionGroupID") retCLong []

-- | Whether this detection track was created by the client.
--
-- ObjC selector: @- userCreated@
userCreated :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO Bool
userCreated cnDetectionTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnDetectionTrack (mkSelector "userCreated") retCULong []

-- | Whether this detection track has discrete detections (otherwise continuous).
--
-- A discrete detection track will return detections only at the specific times a detection occurs. A continuous detection track will return a detection for any requested time and an empty array for time ranges.
--
-- ObjC selector: @- discrete@
discrete :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO Bool
discrete cnDetectionTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnDetectionTrack (mkSelector "discrete") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @detectionType@
detectionTypeSelector :: Selector
detectionTypeSelector = mkSelector "detectionType"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector
detectionGroupIDSelector = mkSelector "detectionGroupID"

-- | @Selector@ for @userCreated@
userCreatedSelector :: Selector
userCreatedSelector = mkSelector "userCreated"

-- | @Selector@ for @discrete@
discreteSelector :: Selector
discreteSelector = mkSelector "discrete"

