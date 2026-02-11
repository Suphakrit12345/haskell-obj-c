{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSpeechMetrics@.
module ObjC.SensorKit.SRSpeechMetrics
  ( SRSpeechMetrics
  , IsSRSpeechMetrics(..)
  , init_
  , new
  , sessionIdentifier
  , sessionFlags
  , timestamp
  , timeSinceAudioStart
  , audioLevel
  , speechExpression
  , initSelector
  , newSelector
  , sessionIdentifierSelector
  , sessionFlagsSelector
  , timestampSelector
  , timeSinceAudioStartSelector
  , audioLevelSelector
  , speechExpressionSelector

  -- * Enum types
  , SRSpeechMetricsSessionFlags(SRSpeechMetricsSessionFlags)
  , pattern SRSpeechMetricsSessionFlagsDefault
  , pattern SRSpeechMetricsSessionFlagsBypassVoiceProcessing

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
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRSpeechMetrics)
init_ srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRSpeechMetrics)
new  =
  do
    cls' <- getRequiredClass "SRSpeechMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | sessionIdentifier
--
-- Identifier of an audio session e.g., a Phone call or Siri utterance
--
-- ObjC selector: @- sessionIdentifier@
sessionIdentifier :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id NSString)
sessionIdentifier srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "sessionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sessionFlags@
sessionFlags :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO SRSpeechMetricsSessionFlags
sessionFlags srSpeechMetrics  =
    fmap (coerce :: CULong -> SRSpeechMetricsSessionFlags) $ sendMsg srSpeechMetrics (mkSelector "sessionFlags") retCULong []

-- | timestamp
--
-- The wall time when this sample was generated
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id NSDate)
timestamp srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | timeSinceAudioStart
--
-- The number of seconds since the start of the audio stream
--
-- When an audio stream like a phone call starts, @SRSpeechMetrics@ samples are collected periodically. This field can be used to determine where each sample falls in the audio stream
--
-- ObjC selector: @- timeSinceAudioStart@
timeSinceAudioStart :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO CDouble
timeSinceAudioStart srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "timeSinceAudioStart") retCDouble []

-- | @- audioLevel@
audioLevel :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRAudioLevel)
audioLevel srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "audioLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- speechExpression@
speechExpression :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRSpeechExpression)
speechExpression srSpeechMetrics  =
    sendMsg srSpeechMetrics (mkSelector "speechExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sessionIdentifier@
sessionIdentifierSelector :: Selector
sessionIdentifierSelector = mkSelector "sessionIdentifier"

-- | @Selector@ for @sessionFlags@
sessionFlagsSelector :: Selector
sessionFlagsSelector = mkSelector "sessionFlags"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @timeSinceAudioStart@
timeSinceAudioStartSelector :: Selector
timeSinceAudioStartSelector = mkSelector "timeSinceAudioStart"

-- | @Selector@ for @audioLevel@
audioLevelSelector :: Selector
audioLevelSelector = mkSelector "audioLevel"

-- | @Selector@ for @speechExpression@
speechExpressionSelector :: Selector
speechExpressionSelector = mkSelector "speechExpression"

