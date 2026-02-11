{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramSession@.
module ObjC.SensorKit.SRElectrocardiogramSession
  ( SRElectrocardiogramSession
  , IsSRElectrocardiogramSession(..)
  , init_
  , new
  , state
  , sessionGuidance
  , identifier
  , initSelector
  , newSelector
  , stateSelector
  , sessionGuidanceSelector
  , identifierSelector

  -- * Enum types
  , SRElectrocardiogramSessionGuidance(SRElectrocardiogramSessionGuidance)
  , pattern SRElectrocardiogramSessionGuidanceGuided
  , pattern SRElectrocardiogramSessionGuidanceUnguided
  , SRElectrocardiogramSessionState(SRElectrocardiogramSessionState)
  , pattern SRElectrocardiogramSessionStateBegin
  , pattern SRElectrocardiogramSessionStateActive
  , pattern SRElectrocardiogramSessionStateEnd

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
init_ :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO (Id SRElectrocardiogramSession)
init_ srElectrocardiogramSession  =
    sendMsg srElectrocardiogramSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRElectrocardiogramSession)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | state
--
-- The state of the ECG session when the sample was recorded
--
-- ObjC selector: @- state@
state :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO SRElectrocardiogramSessionState
state srElectrocardiogramSession  =
    fmap (coerce :: CLong -> SRElectrocardiogramSessionState) $ sendMsg srElectrocardiogramSession (mkSelector "state") retCLong []

-- | sessionGuidance
--
-- The type of session guidance during the the ECG session
--
-- ObjC selector: @- sessionGuidance@
sessionGuidance :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO SRElectrocardiogramSessionGuidance
sessionGuidance srElectrocardiogramSession  =
    fmap (coerce :: CLong -> SRElectrocardiogramSessionGuidance) $ sendMsg srElectrocardiogramSession (mkSelector "sessionGuidance") retCLong []

-- | identifier
--
-- Used to tie samples across multiple @SRFetchResult@ s to the same session
--
-- ObjC selector: @- identifier@
identifier :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO (Id NSString)
identifier srElectrocardiogramSession  =
    sendMsg srElectrocardiogramSession (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @sessionGuidance@
sessionGuidanceSelector :: Selector
sessionGuidanceSelector = mkSelector "sessionGuidance"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

