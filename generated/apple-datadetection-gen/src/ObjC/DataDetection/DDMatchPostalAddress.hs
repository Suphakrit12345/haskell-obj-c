{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a postal address that the data detection system matches.
--
-- The DataDetection framework returns a postal address match in a @DDMatchPostalAddress@ object, which optionally contains the matching parts of a postal address: street, city, state, postal code, and country.
--
-- Generated bindings for @DDMatchPostalAddress@.
module ObjC.DataDetection.DDMatchPostalAddress
  ( DDMatchPostalAddress
  , IsDDMatchPostalAddress(..)
  , street
  , city
  , state
  , postalCode
  , country
  , streetSelector
  , citySelector
  , stateSelector
  , postalCodeSelector
  , countrySelector


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

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The street name in a postal address.
--
-- ObjC selector: @- street@
street :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
street ddMatchPostalAddress  =
    sendMsg ddMatchPostalAddress (mkSelector "street") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The city name in a postal address.
--
-- ObjC selector: @- city@
city :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
city ddMatchPostalAddress  =
    sendMsg ddMatchPostalAddress (mkSelector "city") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The state name in a postal address.
--
-- ObjC selector: @- state@
state :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
state ddMatchPostalAddress  =
    sendMsg ddMatchPostalAddress (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The postal code in a postal address.
--
-- ObjC selector: @- postalCode@
postalCode :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
postalCode ddMatchPostalAddress  =
    sendMsg ddMatchPostalAddress (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The country or region name in a postal address.
--
-- ObjC selector: @- country@
country :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
country ddMatchPostalAddress  =
    sendMsg ddMatchPostalAddress (mkSelector "country") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @street@
streetSelector :: Selector
streetSelector = mkSelector "street"

-- | @Selector@ for @city@
citySelector :: Selector
citySelector = mkSelector "city"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @country@
countrySelector :: Selector
countrySelector = mkSelector "country"

