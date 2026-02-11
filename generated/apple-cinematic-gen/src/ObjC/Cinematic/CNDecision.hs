{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a decision to focus on a specific detectionID or detectionGroupID; optionally strong.
--
-- A strong decision keeps focus for as long as possible.
--
-- Generated bindings for @CNDecision@.
module ObjC.Cinematic.CNDecision
  ( CNDecision
  , IsCNDecision(..)
  , init_
  , new
  , detectionID
  , detectionGroupID
  , userDecision
  , groupDecision
  , strongDecision
  , initSelector
  , newSelector
  , detectionIDSelector
  , detectionGroupIDSelector
  , userDecisionSelector
  , groupDecisionSelector
  , strongDecisionSelector


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

-- | @- init@
init_ :: IsCNDecision cnDecision => cnDecision -> IO (Id CNDecision)
init_ cnDecision  =
    sendMsg cnDecision (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNDecision)
new  =
  do
    cls' <- getRequiredClass "CNDecision"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The detectionID of the detection to focus on if this is not a group decision.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDecision cnDecision => cnDecision -> IO CLong
detectionID cnDecision  =
    sendMsg cnDecision (mkSelector "detectionID") retCLong []

-- | The detectionGroupID of the detection to focus on if this is a group decision.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDecision cnDecision => cnDecision -> IO CLong
detectionGroupID cnDecision  =
    sendMsg cnDecision (mkSelector "detectionGroupID") retCLong []

-- | Whether this is a user-created decision, or a base decision.
--
-- ObjC selector: @- userDecision@
userDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
userDecision cnDecision  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnDecision (mkSelector "userDecision") retCULong []

-- | Whether this is a group decision or not.
--
-- ObjC selector: @- groupDecision@
groupDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
groupDecision cnDecision  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnDecision (mkSelector "groupDecision") retCULong []

-- | Whether this is a strong decision or not. A strong decision keeps focus for as long as possible.
--
-- ObjC selector: @- strongDecision@
strongDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
strongDecision cnDecision  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnDecision (mkSelector "strongDecision") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector
detectionGroupIDSelector = mkSelector "detectionGroupID"

-- | @Selector@ for @userDecision@
userDecisionSelector :: Selector
userDecisionSelector = mkSelector "userDecision"

-- | @Selector@ for @groupDecision@
groupDecisionSelector :: Selector
groupDecisionSelector = mkSelector "groupDecision"

-- | @Selector@ for @strongDecision@
strongDecisionSelector :: Selector
strongDecisionSelector = mkSelector "strongDecision"

