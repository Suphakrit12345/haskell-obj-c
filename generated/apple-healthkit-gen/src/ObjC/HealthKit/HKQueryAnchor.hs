{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQueryAnchor
--
-- This object encapsulates the state of an HKAnchoredObjectQuery
--
-- Generated bindings for @HKQueryAnchor@.
module ObjC.HealthKit.HKQueryAnchor
  ( HKQueryAnchor
  , IsHKQueryAnchor(..)
  , anchorFromValue
  , init_
  , anchorFromValueSelector
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

-- | anchorFromValue:
--
-- Creates an HKQueryAnchor with an integer anchor which was previously obtained from an                HKAnchoredObjectQuery prior to iOS 9.0.
--
-- ObjC selector: @+ anchorFromValue:@
anchorFromValue :: CULong -> IO (Id HKQueryAnchor)
anchorFromValue value =
  do
    cls' <- getRequiredClass "HKQueryAnchor"
    sendClassMsg cls' (mkSelector "anchorFromValue:") (retPtr retVoid) [argCULong value] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKQueryAnchor hkQueryAnchor => hkQueryAnchor -> IO (Id HKQueryAnchor)
init_ hkQueryAnchor  =
    sendMsg hkQueryAnchor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorFromValue:@
anchorFromValueSelector :: Selector
anchorFromValueSelector = mkSelector "anchorFromValue:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

