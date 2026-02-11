{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A session configuration that enables UWB Down Link Time Difference of Arrival(DL-TDoA) ranging with nearby anchors.
--
-- Generated bindings for @NIDLTDOAConfiguration@.
module ObjC.NearbyInteraction.NIDLTDOAConfiguration
  ( NIDLTDOAConfiguration
  , IsNIDLTDOAConfiguration(..)
  , initWithNetworkIdentifier
  , init_
  , new
  , networkIdentifier
  , setNetworkIdentifier
  , initWithNetworkIdentifierSelector
  , initSelector
  , newSelector
  , networkIdentifierSelector
  , setNetworkIdentifierSelector


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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new configuration with a network identifier
--
-- ObjC selector: @- initWithNetworkIdentifier:@
initWithNetworkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> CLong -> IO (Id NIDLTDOAConfiguration)
initWithNetworkIdentifier nidltdoaConfiguration  networkIdentifier =
    sendMsg nidltdoaConfiguration (mkSelector "initWithNetworkIdentifier:") (retPtr retVoid) [argCLong networkIdentifier] >>= ownedObject . castPtr

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> IO (Id NIDLTDOAConfiguration)
init_ nidltdoaConfiguration  =
    sendMsg nidltdoaConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NIDLTDOAConfiguration)
new  =
  do
    cls' <- getRequiredClass "NIDLTDOAConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A unique identifier for a network supporting UWB Down Link Time Difference of Arrival(DL-TDoA).
--
-- ObjC selector: @- networkIdentifier@
networkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> IO CLong
networkIdentifier nidltdoaConfiguration  =
    sendMsg nidltdoaConfiguration (mkSelector "networkIdentifier") retCLong []

-- | A unique identifier for a network supporting UWB Down Link Time Difference of Arrival(DL-TDoA).
--
-- ObjC selector: @- setNetworkIdentifier:@
setNetworkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> CLong -> IO ()
setNetworkIdentifier nidltdoaConfiguration  value =
    sendMsg nidltdoaConfiguration (mkSelector "setNetworkIdentifier:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNetworkIdentifier:@
initWithNetworkIdentifierSelector :: Selector
initWithNetworkIdentifierSelector = mkSelector "initWithNetworkIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @networkIdentifier@
networkIdentifierSelector :: Selector
networkIdentifierSelector = mkSelector "networkIdentifier"

-- | @Selector@ for @setNetworkIdentifier:@
setNetworkIdentifierSelector :: Selector
setNetworkIdentifierSelector = mkSelector "setNetworkIdentifier:"

