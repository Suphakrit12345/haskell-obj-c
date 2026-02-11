{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPlacemark@.
module ObjC.MapKit.MKPlacemark
  ( MKPlacemark
  , IsMKPlacemark(..)
  , countryCode
  , countryCodeSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Contacts.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- countryCode@
countryCode :: IsMKPlacemark mkPlacemark => mkPlacemark -> IO (Id NSString)
countryCode mkPlacemark  =
    sendMsg mkPlacemark (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

