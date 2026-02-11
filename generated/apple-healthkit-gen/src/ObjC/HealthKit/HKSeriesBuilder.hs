{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSeriesBuilder
--
-- An HKSeriesBuilder is an abstract class for generating HKSeriesSample objects.                 Concrete subclasses generate the actual HKSeriesSample objects.
--
-- Generated bindings for @HKSeriesBuilder@.
module ObjC.HealthKit.HKSeriesBuilder
  ( HKSeriesBuilder
  , IsHKSeriesBuilder(..)
  , discard
  , init_
  , discardSelector
  , initSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | discard
--
-- Stop series generation and discard all collected data.
--
-- This method informs the receiver that no more data should be collected and all                previously collected data should be deleted and the receiver will be considered                invalid. Any further calls to the receiver will result in an exception.
--
-- ObjC selector: @- discard@
discard :: IsHKSeriesBuilder hkSeriesBuilder => hkSeriesBuilder -> IO ()
discard hkSeriesBuilder  =
    sendMsg hkSeriesBuilder (mkSelector "discard") retVoid []

-- | init
--
-- Use only subclass initializer methods.
--
-- ObjC selector: @- init@
init_ :: IsHKSeriesBuilder hkSeriesBuilder => hkSeriesBuilder -> IO (Id HKSeriesBuilder)
init_ hkSeriesBuilder  =
    sendMsg hkSeriesBuilder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @discard@
discardSelector :: Selector
discardSelector = mkSelector "discard"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

