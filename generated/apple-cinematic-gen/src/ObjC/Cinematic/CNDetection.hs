{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A cinematic detection of a subject.
--
-- Specifies the type, distance (as disparity), bounds (as a normalized rectangle), and time (as CMTime) of the detection. Detections obtained from the cinematic script include a detectionID that can be used to track the detection over time. Some types of detections also include a detectionGroupID that associates related detections (e.g. the face and torso of the same person).
--
-- Generated bindings for @CNDetection@.
module ObjC.Cinematic.CNDetection
  ( CNDetection
  , IsCNDetection(..)
  , isValidDetectionID
  , isValidDetectionGroupID
  , accessibilityLabelForDetectionType
  , init_
  , new
  , detectionType
  , focusDisparity
  , detectionID
  , detectionGroupID
  , isValidDetectionIDSelector
  , isValidDetectionGroupIDSelector
  , accessibilityLabelForDetectionTypeSelector
  , initSelector
  , newSelector
  , detectionTypeSelector
  , focusDisparitySelector
  , detectionIDSelector
  , detectionGroupIDSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Cinematic.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Determine whether a given detectionID is valid
--
-- ObjC selector: @+ isValidDetectionID:@
isValidDetectionID :: CLong -> IO Bool
isValidDetectionID detectionID =
  do
    cls' <- getRequiredClass "CNDetection"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isValidDetectionID:") retCULong [argCLong detectionID]

-- | Determine whether a given detectionGroupID is valid
--
-- ObjC selector: @+ isValidDetectionGroupID:@
isValidDetectionGroupID :: CLong -> IO Bool
isValidDetectionGroupID detectionGroupID =
  do
    cls' <- getRequiredClass "CNDetection"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isValidDetectionGroupID:") retCULong [argCLong detectionGroupID]

-- | A localized accessibility label converting a specific detection type into a broad category (person, pet, etc.).
--
-- ObjC selector: @+ accessibilityLabelForDetectionType:@
accessibilityLabelForDetectionType :: CNDetectionType -> IO (Id NSString)
accessibilityLabelForDetectionType detectionType =
  do
    cls' <- getRequiredClass "CNDetection"
    sendClassMsg cls' (mkSelector "accessibilityLabelForDetectionType:") (retPtr retVoid) [argCLong (coerce detectionType)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCNDetection cnDetection => cnDetection -> IO (Id CNDetection)
init_ cnDetection  =
    sendMsg cnDetection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNDetection)
new  =
  do
    cls' <- getRequiredClass "CNDetection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The type of object that was detected (face, torso, cat, dog, etc.)
--
-- ObjC selector: @- detectionType@
detectionType :: IsCNDetection cnDetection => cnDetection -> IO CNDetectionType
detectionType cnDetection  =
    fmap (coerce :: CLong -> CNDetectionType) $ sendMsg cnDetection (mkSelector "detectionType") retCLong []

-- | The disparity to use in order to focus on the object. If the disparity is unknown, use the class method to find it: @disparityInNormalizedRect:sourceDisparity:detectionType:priorDisparity:@.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNDetection cnDetection => cnDetection -> IO CFloat
focusDisparity cnDetection  =
    sendMsg cnDetection (mkSelector "focusDisparity") retCFloat []

-- | An unique identifier assigned by the cinematic script to all detections of the same subject and detection type across time. If you build a custom detection track, the detectionID will be assigned when you add it to the script.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDetection cnDetection => cnDetection -> IO CLong
detectionID cnDetection  =
    sendMsg cnDetection (mkSelector "detectionID") retCLong []

-- | An unique identifier assigned by the cinematic script to all detections of the same subject and related detection types across time. For example, the face/torso detections of the same person are assigned the same detectionGroupID.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDetection cnDetection => cnDetection -> IO CLong
detectionGroupID cnDetection  =
    sendMsg cnDetection (mkSelector "detectionGroupID") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isValidDetectionID:@
isValidDetectionIDSelector :: Selector
isValidDetectionIDSelector = mkSelector "isValidDetectionID:"

-- | @Selector@ for @isValidDetectionGroupID:@
isValidDetectionGroupIDSelector :: Selector
isValidDetectionGroupIDSelector = mkSelector "isValidDetectionGroupID:"

-- | @Selector@ for @accessibilityLabelForDetectionType:@
accessibilityLabelForDetectionTypeSelector :: Selector
accessibilityLabelForDetectionTypeSelector = mkSelector "accessibilityLabelForDetectionType:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @detectionType@
detectionTypeSelector :: Selector
detectionTypeSelector = mkSelector "detectionType"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector
detectionGroupIDSelector = mkSelector "detectionGroupID"

