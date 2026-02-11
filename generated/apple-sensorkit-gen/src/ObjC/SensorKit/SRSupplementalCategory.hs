{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SRSupplementalCategory
--
-- A supplemental category to provide more context than just the app category
--
-- The app categories from @SRDeviceUsageCategoryKey@ are very general. Providing a supplemental category allows more context about the specific app while not revealing the exact app identity.
--
-- Generated bindings for @SRSupplementalCategory@.
module ObjC.SensorKit.SRSupplementalCategory
  ( SRSupplementalCategory
  , IsSRSupplementalCategory(..)
  , init_
  , new
  , identifier
  , initSelector
  , newSelector
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
init_ :: IsSRSupplementalCategory srSupplementalCategory => srSupplementalCategory -> IO (Id SRSupplementalCategory)
init_ srSupplementalCategory  =
    sendMsg srSupplementalCategory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRSupplementalCategory)
new  =
  do
    cls' <- getRequiredClass "SRSupplementalCategory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- An opaque identifier for the supplemental category
--
-- More information about what this category represents can be found in Apple's developer documentation
--
-- ObjC selector: @- identifier@
identifier :: IsSRSupplementalCategory srSupplementalCategory => srSupplementalCategory -> IO (Id NSString)
identifier srSupplementalCategory  =
    sendMsg srSupplementalCategory (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

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

