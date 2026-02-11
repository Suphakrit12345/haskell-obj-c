{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Representation of metric data corresponding to a metric event.
--
-- Generated bindings for @MTRMetricData@.
module ObjC.Matter.MTRMetricData
  ( MTRMetricData
  , IsMTRMetricData(..)
  , value
  , errorCode
  , duration
  , valueSelector
  , errorCodeSelector
  , durationSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Value for the metric data. The value may be nil depending on the event emitted.
--
-- ObjC selector: @- value@
value :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
value mtrMetricData  =
    sendMsg mtrMetricData (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Error code for the metric data. This value, when not nil, holds the error code value of the operation associated with the event. Interpretation of the error code value dependents on the metric being emitted.
--
-- ObjC selector: @- errorCode@
errorCode :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
errorCode mtrMetricData  =
    sendMsg mtrMetricData (mkSelector "errorCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Duration of event associated with the metric. This value may be nil depending on the event emitted. When not nil, the value of duration is of type NSTimeInterval.
--
-- ObjC selector: @- duration@
duration :: IsMTRMetricData mtrMetricData => mtrMetricData -> IO (Id NSNumber)
duration mtrMetricData  =
    sendMsg mtrMetricData (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @errorCode@
errorCodeSelector :: Selector
errorCodeSelector = mkSelector "errorCode"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

