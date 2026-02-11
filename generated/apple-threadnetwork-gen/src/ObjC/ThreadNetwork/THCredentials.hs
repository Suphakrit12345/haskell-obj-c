{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that contains credentials for a Thread network.
--
-- A Thread network defines parameters that all connected devices use. ``THCredentials`` provides these parameters.
--
-- Generated bindings for @THCredentials@.
module ObjC.ThreadNetwork.THCredentials
  ( THCredentials
  , IsTHCredentials(..)
  , init_
  , new
  , networkName
  , extendedPANID
  , borderAgentID
  , activeOperationalDataSet
  , networkKey
  , pskc
  , channel
  , setChannel
  , panID
  , creationDate
  , lastModificationDate
  , initSelector
  , newSelector
  , networkNameSelector
  , extendedPANIDSelector
  , borderAgentIDSelector
  , activeOperationalDataSetSelector
  , networkKeySelector
  , pskcSelector
  , channelSelector
  , setChannelSelector
  , panIDSelector
  , creationDateSelector
  , lastModificationDateSelector


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

import ObjC.ThreadNetwork.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTHCredentials thCredentials => thCredentials -> IO (Id THCredentials)
init_ thCredentials  =
    sendMsg thCredentials (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id THCredentials)
new  =
  do
    cls' <- getRequiredClass "THCredentials"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Thread network name.
--
-- ObjC selector: @- networkName@
networkName :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSString)
networkName thCredentials  =
    sendMsg thCredentials (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread network extended PAN identifier.
--
-- ObjC selector: @- extendedPANID@
extendedPANID :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
extendedPANID thCredentials  =
    sendMsg thCredentials (mkSelector "extendedPANID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifer of an active Thread network Border Agent.
--
-- This property’s value is the MAC Extended Address, a random identifier that the active Thread network border router generates.
--
-- ObjC selector: @- borderAgentID@
borderAgentID :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
borderAgentID thCredentials  =
    sendMsg thCredentials (mkSelector "borderAgentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The essential operational parameters for the Thread network.
--
-- The framework parses this property, then extracts and sets ``THCredentials/channel``, ``THCredentials/extendedPANID``, ``THCredentials/networkKey``, ``THCredentials/networkName``, ``THCredentials/panID``, and ``THCredentials/PSKC`` when you call ``THClient/storeCredentialsForBorderAgent:activeOperationalDataSet:completion:``.
--
-- ObjC selector: @- activeOperationalDataSet@
activeOperationalDataSet :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
activeOperationalDataSet thCredentials  =
    sendMsg thCredentials (mkSelector "activeOperationalDataSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The sixteen byte Thread network key.
--
-- ObjC selector: @- networkKey@
networkKey :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
networkKey thCredentials  =
    sendMsg thCredentials (mkSelector "networkKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The sixteen byte Thread network pre-shared key for the Commissioner.
--
-- ObjC selector: @- PSKC@
pskc :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
pskc thCredentials  =
    sendMsg thCredentials (mkSelector "PSKC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread network radio channel.
--
-- ObjC selector: @- channel@
channel :: IsTHCredentials thCredentials => thCredentials -> IO CUChar
channel thCredentials  =
    sendMsg thCredentials (mkSelector "channel") retCUChar []

-- | The Thread network radio channel.
--
-- ObjC selector: @- setChannel:@
setChannel :: IsTHCredentials thCredentials => thCredentials -> CUChar -> IO ()
setChannel thCredentials  value =
    sendMsg thCredentials (mkSelector "setChannel:") retVoid [argCUChar value]

-- | The two byte Thead network PAN identifier.
--
-- ObjC selector: @- panID@
panID :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSData)
panID thCredentials  =
    sendMsg thCredentials (mkSelector "panID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time that the framework stored the credential in the database.
--
-- ObjC selector: @- creationDate@
creationDate :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSDate)
creationDate thCredentials  =
    sendMsg thCredentials (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time that the framework updated the credential in the database.
--
-- ObjC selector: @- lastModificationDate@
lastModificationDate :: IsTHCredentials thCredentials => thCredentials -> IO (Id NSDate)
lastModificationDate thCredentials  =
    sendMsg thCredentials (mkSelector "lastModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @extendedPANID@
extendedPANIDSelector :: Selector
extendedPANIDSelector = mkSelector "extendedPANID"

-- | @Selector@ for @borderAgentID@
borderAgentIDSelector :: Selector
borderAgentIDSelector = mkSelector "borderAgentID"

-- | @Selector@ for @activeOperationalDataSet@
activeOperationalDataSetSelector :: Selector
activeOperationalDataSetSelector = mkSelector "activeOperationalDataSet"

-- | @Selector@ for @networkKey@
networkKeySelector :: Selector
networkKeySelector = mkSelector "networkKey"

-- | @Selector@ for @PSKC@
pskcSelector :: Selector
pskcSelector = mkSelector "PSKC"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @panID@
panIDSelector :: Selector
panIDSelector = mkSelector "panID"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @lastModificationDate@
lastModificationDateSelector :: Selector
lastModificationDateSelector = mkSelector "lastModificationDate"

