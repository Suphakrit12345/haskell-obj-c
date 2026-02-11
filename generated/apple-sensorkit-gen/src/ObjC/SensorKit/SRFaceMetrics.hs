{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFaceMetrics@.
module ObjC.SensorKit.SRFaceMetrics
  ( SRFaceMetrics
  , IsSRFaceMetrics(..)
  , init_
  , new
  , version
  , sessionIdentifier
  , context
  , wholeFaceExpressions
  , partialFaceExpressions
  , initSelector
  , newSelector
  , versionSelector
  , sessionIdentifierSelector
  , contextSelector
  , wholeFaceExpressionsSelector
  , partialFaceExpressionsSelector

  -- * Enum types
  , SRFaceMetricsContext(SRFaceMetricsContext)
  , pattern SRFaceMetricsContextDeviceUnlock
  , pattern SRFaceMetricsContextMessagingAppUsage

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
init_ :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id SRFaceMetrics)
init_ srFaceMetrics  =
    sendMsg srFaceMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRFaceMetrics)
new  =
  do
    cls' <- getRequiredClass "SRFaceMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | version
--
-- Algorithm version
--
-- ObjC selector: @- version@
version :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSString)
version srFaceMetrics  =
    sendMsg srFaceMetrics (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sessionIdentifier
--
-- Identifier of a camera session
--
-- ObjC selector: @- sessionIdentifier@
sessionIdentifier :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSString)
sessionIdentifier srFaceMetrics  =
    sendMsg srFaceMetrics (mkSelector "sessionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | context
--
-- Indicates system context during a camera session, e.g., if the device was unlocked or (and) a messaging app was used
--
-- ObjC selector: @- context@
context :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO SRFaceMetricsContext
context srFaceMetrics  =
    fmap (coerce :: CULong -> SRFaceMetricsContext) $ sendMsg srFaceMetrics (mkSelector "context") retCULong []

-- | wholeFaceExpressions
--
-- Detected whole face expressions
--
-- ObjC selector: @- wholeFaceExpressions@
wholeFaceExpressions :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSArray)
wholeFaceExpressions srFaceMetrics  =
    sendMsg srFaceMetrics (mkSelector "wholeFaceExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | partialFaceExpressions
--
-- Detected partial face expressions
--
-- ObjC selector: @- partialFaceExpressions@
partialFaceExpressions :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSArray)
partialFaceExpressions srFaceMetrics  =
    sendMsg srFaceMetrics (mkSelector "partialFaceExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @sessionIdentifier@
sessionIdentifierSelector :: Selector
sessionIdentifierSelector = mkSelector "sessionIdentifier"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @wholeFaceExpressions@
wholeFaceExpressionsSelector :: Selector
wholeFaceExpressionsSelector = mkSelector "wholeFaceExpressions"

-- | @Selector@ for @partialFaceExpressions@
partialFaceExpressionsSelector :: Selector
partialFaceExpressionsSelector = mkSelector "partialFaceExpressions"

