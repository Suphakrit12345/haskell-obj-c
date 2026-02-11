{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PGDisplayMode:
--
-- Description of supported display mode.
--
-- Client of PGDisplay can dynamically supply NSArray of PGDisplayMode objects to convey supported modes.  The first mode in array is preferred.
--
-- Generated bindings for @PGDisplayMode@.
module ObjC.ParavirtualizedGraphics.PGDisplayMode
  ( PGDisplayMode
  , IsPGDisplayMode(..)
  , refreshRate
  , refreshRateSelector


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

import ObjC.ParavirtualizedGraphics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | refreshRateInHz
--
-- refreshRate of supported display mode.  Consider only supplying modes using a refreshRate equal to that of host OS's physical display where representation is ultimately shown.
--
-- ObjC selector: @- refreshRate@
refreshRate :: IsPGDisplayMode pgDisplayMode => pgDisplayMode -> IO CDouble
refreshRate pgDisplayMode  =
    sendMsg pgDisplayMode (mkSelector "refreshRate") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @refreshRate@
refreshRateSelector :: Selector
refreshRateSelector = mkSelector "refreshRate"

