{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This object describes a Crash Detection event and response to it.
--
-- SACrashDetectionEvent
--
-- Generated bindings for @SACrashDetectionEvent@.
module ObjC.SafetyKit.SACrashDetectionEvent
  ( SACrashDetectionEvent
  , IsSACrashDetectionEvent(..)
  , new
  , init_
  , date
  , response
  , newSelector
  , initSelector
  , dateSelector
  , responseSelector

  -- * Enum types
  , SACrashDetectionEventResponse(SACrashDetectionEventResponse)
  , pattern SACrashDetectionEventResponseAttempted
  , pattern SACrashDetectionEventResponseDisabled

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

import ObjC.SafetyKit.Internal.Classes
import ObjC.SafetyKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SACrashDetectionEvent)
new  =
  do
    cls' <- getRequiredClass "SACrashDetectionEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO (Id SACrashDetectionEvent)
init_ saCrashDetectionEvent  =
    sendMsg saCrashDetectionEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | date
--
-- The time a crash was detected
--
-- ObjC selector: @- date@
date :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO (Id NSDate)
date saCrashDetectionEvent  =
    sendMsg saCrashDetectionEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | response
--
-- enum value representing the emergency response to the Crash Detection event
--
-- SACrashDetectionEventResponse
--
-- ObjC selector: @- response@
response :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO SACrashDetectionEventResponse
response saCrashDetectionEvent  =
    fmap (coerce :: CLong -> SACrashDetectionEventResponse) $ sendMsg saCrashDetectionEvent (mkSelector "response") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

