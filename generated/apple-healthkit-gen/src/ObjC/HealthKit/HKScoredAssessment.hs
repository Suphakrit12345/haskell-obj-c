{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKScoredAssessment
--
-- An abstract HKSample subclass representing the results of a scored assessment.
--
-- Generated bindings for @HKScoredAssessment@.
module ObjC.HealthKit.HKScoredAssessment
  ( HKScoredAssessment
  , IsHKScoredAssessment(..)
  , init_
  , new
  , score
  , initSelector
  , newSelector
  , scoreSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKScoredAssessment hkScoredAssessment => hkScoredAssessment -> IO (Id HKScoredAssessment)
init_ hkScoredAssessment  =
    sendMsg hkScoredAssessment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKScoredAssessment)
new  =
  do
    cls' <- getRequiredClass "HKScoredAssessment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The score determined by the answers on an assessment
--
-- ObjC selector: @- score@
score :: IsHKScoredAssessment hkScoredAssessment => hkScoredAssessment -> IO CLong
score hkScoredAssessment  =
    sendMsg hkScoredAssessment (mkSelector "score") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @score@
scoreSelector :: Selector
scoreSelector = mkSelector "score"

