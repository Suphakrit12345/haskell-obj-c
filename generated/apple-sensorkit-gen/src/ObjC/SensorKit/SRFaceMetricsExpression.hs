{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFaceMetricsExpression@.
module ObjC.SensorKit.SRFaceMetricsExpression
  ( SRFaceMetricsExpression
  , IsSRFaceMetricsExpression(..)
  , init_
  , new
  , identifier
  , value
  , initSelector
  , newSelector
  , identifierSelector
  , valueSelector


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
init_ :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO (Id SRFaceMetricsExpression)
init_ srFaceMetricsExpression  =
    sendMsg srFaceMetricsExpression (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRFaceMetricsExpression)
new  =
  do
    cls' <- getRequiredClass "SRFaceMetricsExpression"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- An opaque identifier for the face expression
--
-- More information about what this face expression represents can be found in Apple's developer documentation
--
-- ObjC selector: @- identifier@
identifier :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO (Id NSString)
identifier srFaceMetricsExpression  =
    sendMsg srFaceMetricsExpression (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- double value indicating the current position of the expression
--
-- ObjC selector: @- value@
value :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO CDouble
value srFaceMetricsExpression  =
    sendMsg srFaceMetricsExpression (mkSelector "value") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

