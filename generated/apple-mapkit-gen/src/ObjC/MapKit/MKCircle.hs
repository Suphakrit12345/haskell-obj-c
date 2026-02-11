{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKCircle@.
module ObjC.MapKit.MKCircle
  ( MKCircle
  , IsMKCircle(..)
  , radius
  , radiusSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- radius@
radius :: IsMKCircle mkCircle => mkCircle -> IO CDouble
radius mkCircle  =
    sendMsg mkCircle (mkSelector "radius") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

