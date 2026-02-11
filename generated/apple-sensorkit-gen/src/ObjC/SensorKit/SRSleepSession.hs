{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSleepSession@.
module ObjC.SensorKit.SRSleepSession
  ( SRSleepSession
  , IsSRSleepSession(..)
  , init_
  , new
  , startDate
  , duration
  , identifier
  , initSelector
  , newSelector
  , startDateSelector
  , durationSelector
  , identifierSelector


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
init_ :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id SRSleepSession)
init_ srSleepSession  =
    sendMsg srSleepSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRSleepSession)
new  =
  do
    cls' <- getRequiredClass "SRSleepSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | startDate
--
-- Start date of sleep session
--
-- ObjC selector: @- startDate@
startDate :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id NSDate)
startDate srSleepSession  =
    sendMsg srSleepSession (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | duration
--
-- Sleep session duration
--
-- Equal to 0 if endReason is SRSleepSessionEndReasonNoEndEvent
--
-- ObjC selector: @- duration@
duration :: IsSRSleepSession srSleepSession => srSleepSession -> IO CDouble
duration srSleepSession  =
    sendMsg srSleepSession (mkSelector "duration") retCDouble []

-- | identifier
--
-- Sleep session unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id NSString)
identifier srSleepSession  =
    sendMsg srSleepSession (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

