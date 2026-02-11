{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a (vendor, product) pair that identifies a specific product.
--
-- Generated bindings for @MTRProductIdentity@.
module ObjC.Matter.MTRProductIdentity
  ( MTRProductIdentity
  , IsMTRProductIdentity(..)
  , init_
  , new
  , initWithVendorID_productID
  , vendorID
  , productID
  , initSelector
  , newSelector
  , initWithVendorID_productIDSelector
  , vendorIDSelector
  , productIDSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id MTRProductIdentity)
init_ mtrProductIdentity  =
    sendMsg mtrProductIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRProductIdentity)
new  =
  do
    cls' <- getRequiredClass "MTRProductIdentity"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithVendorID:productID:@
initWithVendorID_productID :: (IsMTRProductIdentity mtrProductIdentity, IsNSNumber vendorID, IsNSNumber productID) => mtrProductIdentity -> vendorID -> productID -> IO (Id MTRProductIdentity)
initWithVendorID_productID mtrProductIdentity  vendorID productID =
  withObjCPtr vendorID $ \raw_vendorID ->
    withObjCPtr productID $ \raw_productID ->
        sendMsg mtrProductIdentity (mkSelector "initWithVendorID:productID:") (retPtr retVoid) [argPtr (castPtr raw_vendorID :: Ptr ()), argPtr (castPtr raw_productID :: Ptr ())] >>= ownedObject . castPtr

-- | @- vendorID@
vendorID :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id NSNumber)
vendorID mtrProductIdentity  =
    sendMsg mtrProductIdentity (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- productID@
productID :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id NSNumber)
productID mtrProductIdentity  =
    sendMsg mtrProductIdentity (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithVendorID:productID:@
initWithVendorID_productIDSelector :: Selector
initWithVendorID_productIDSelector = mkSelector "initWithVendorID:productID:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

