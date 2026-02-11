{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSpeechExpression@.
module ObjC.SensorKit.SRSpeechExpression
  ( SRSpeechExpression
  , IsSRSpeechExpression(..)
  , init_
  , new
  , version
  , confidence
  , mood
  , valence
  , activation
  , dominance
  , initSelector
  , newSelector
  , versionSelector
  , confidenceSelector
  , moodSelector
  , valenceSelector
  , activationSelector
  , dominanceSelector


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
init_ :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO (Id SRSpeechExpression)
init_ srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRSpeechExpression)
new  =
  do
    cls' <- getRequiredClass "SRSpeechExpression"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | version
--
-- Version of the algorithm used to generate @SRSpeechExpression@
--
-- ObjC selector: @- version@
version :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO (Id NSString)
version srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | confidence
--
-- The level of confidence normalized to [0, 1], where 1 is most confident
--
-- ObjC selector: @- confidence@
confidence :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
confidence srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "confidence") retCDouble []

-- | mood
--
-- Indicator of how slurry/tired/exhausted the speaker sounds as opposed to normal.
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- mood@
mood :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
mood srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "mood") retCDouble []

-- | valence
--
-- Degree of (perceived) positive or negative emotion/sentiment from voice
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- valence@
valence :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
valence srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "valence") retCDouble []

-- | activation
--
-- Level of energy or activation (perceived) in voice
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- activation@
activation :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
activation srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "activation") retCDouble []

-- | dominance
--
-- Degree of how strong or meek a person sounds (perceptually)
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- dominance@
dominance :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
dominance srSpeechExpression  =
    sendMsg srSpeechExpression (mkSelector "dominance") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @mood@
moodSelector :: Selector
moodSelector = mkSelector "mood"

-- | @Selector@ for @valence@
valenceSelector :: Selector
valenceSelector = mkSelector "valence"

-- | @Selector@ for @activation@
activationSelector :: Selector
activationSelector = mkSelector "activation"

-- | @Selector@ for @dominance@
dominanceSelector :: Selector
dominanceSelector = mkSelector "dominance"

