{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CNBoundsPrediction@.
module ObjC.Cinematic.CNBoundsPrediction
  ( CNBoundsPrediction
  , IsCNBoundsPrediction(..)
  , confidence
  , setConfidence
  , confidenceSelector
  , setConfidenceSelector


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
import ObjC.Foundation.Internal.Classes

-- | the probability that a well-defined object is within the bounds — a number between 0.0 and 1.0.
--
-- ObjC selector: @- confidence@
confidence :: IsCNBoundsPrediction cnBoundsPrediction => cnBoundsPrediction -> IO CFloat
confidence cnBoundsPrediction  =
    sendMsg cnBoundsPrediction (mkSelector "confidence") retCFloat []

-- | the probability that a well-defined object is within the bounds — a number between 0.0 and 1.0.
--
-- ObjC selector: @- setConfidence:@
setConfidence :: IsCNBoundsPrediction cnBoundsPrediction => cnBoundsPrediction -> CFloat -> IO ()
setConfidence cnBoundsPrediction  value =
    sendMsg cnBoundsPrediction (mkSelector "setConfidence:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @setConfidence:@
setConfidenceSelector :: Selector
setConfidenceSelector = mkSelector "setConfidence:"

