{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLCircularGeographicCondition@.
module ObjC.CoreLocation.CLCircularGeographicCondition
  ( CLCircularGeographicCondition
  , IsCLCircularGeographicCondition(..)
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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- radius@
radius :: IsCLCircularGeographicCondition clCircularGeographicCondition => clCircularGeographicCondition -> IO CDouble
radius clCircularGeographicCondition  =
    sendMsg clCircularGeographicCondition (mkSelector "radius") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

