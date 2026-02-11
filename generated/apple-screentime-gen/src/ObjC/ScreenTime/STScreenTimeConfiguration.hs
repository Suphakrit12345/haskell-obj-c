{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The configuration for this device.
--
-- Generated bindings for @STScreenTimeConfiguration@.
module ObjC.ScreenTime.STScreenTimeConfiguration
  ( STScreenTimeConfiguration
  , IsSTScreenTimeConfiguration(..)
  , init_
  , new
  , enforcesChildRestrictions
  , initSelector
  , newSelector
  , enforcesChildRestrictionsSelector


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

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSTScreenTimeConfiguration stScreenTimeConfiguration => stScreenTimeConfiguration -> IO (Id STScreenTimeConfiguration)
init_ stScreenTimeConfiguration  =
    sendMsg stScreenTimeConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id STScreenTimeConfiguration)
new  =
  do
    cls' <- getRequiredClass "STScreenTimeConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A Boolean that indicates whether the device is currently enforcing child restrictions.
--
-- ObjC selector: @- enforcesChildRestrictions@
enforcesChildRestrictions :: IsSTScreenTimeConfiguration stScreenTimeConfiguration => stScreenTimeConfiguration -> IO Bool
enforcesChildRestrictions stScreenTimeConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg stScreenTimeConfiguration (mkSelector "enforcesChildRestrictions") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @enforcesChildRestrictions@
enforcesChildRestrictionsSelector :: Selector
enforcesChildRestrictionsSelector = mkSelector "enforcesChildRestrictions"

