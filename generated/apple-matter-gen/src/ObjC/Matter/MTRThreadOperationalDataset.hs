{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRThreadOperationalDataset allows converting between an "expanded" view of the dataset (with the separate fields) and a single-blob NSData view.
--
-- The latter can be used to pass Thread network credentials via MTRCommissioningParameters.
--
-- Generated bindings for @MTRThreadOperationalDataset@.
module ObjC.Matter.MTRThreadOperationalDataset
  ( MTRThreadOperationalDataset
  , IsMTRThreadOperationalDataset(..)
  , init_
  , new
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID
  , initWithData
  , data_
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID
  , networkName
  , extendedPANID
  , masterKey
  , psKc
  , channelNumber
  , panID
  , channel
  , setChannel
  , initSelector
  , newSelector
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector
  , initWithDataSelector
  , dataSelector
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector
  , networkNameSelector
  , extendedPANIDSelector
  , masterKeySelector
  , psKcSelector
  , channelNumberSelector
  , panIDSelector
  , channelSelector
  , setChannelSelector


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
init_ :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id MTRThreadOperationalDataset)
init_ mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRThreadOperationalDataset)
new  =
  do
    cls' <- getRequiredClass "MTRThreadOperationalDataset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Create a Thread Operational Dataset object with the individual network fields.
--
-- @extendedPANID@ — Must be MTRSizeThreadExtendedPANID bytes.  Otherwise nil                      will be returned.
--
-- @masterKey@ — Must be MTRSizeThreadMasterKey bytes. Otherwise nil will be                  returned.
--
-- @PSKc@ — Must be MTRSizeThreadPSKc bytes.  Otherwise nil will be returned.
--
-- @channelNumber@ — Must be an unsigned 16-bit value.
--
-- @panID@ — Must be MTRSizeThreadPANID bytes.  Otherwise nil will be              returned.  In particular, it's expected to be a 16-bit unsigned              integer stored as 2 bytes in host order.
--
-- ObjC selector: @- initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSString networkName, IsNSData extendedPANID, IsNSData masterKey, IsNSData psKc, IsNSNumber channelNumber, IsNSData panID) => mtrThreadOperationalDataset -> networkName -> extendedPANID -> masterKey -> psKc -> channelNumber -> panID -> IO (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID mtrThreadOperationalDataset  networkName extendedPANID masterKey psKc channelNumber panID =
  withObjCPtr networkName $ \raw_networkName ->
    withObjCPtr extendedPANID $ \raw_extendedPANID ->
      withObjCPtr masterKey $ \raw_masterKey ->
        withObjCPtr psKc $ \raw_psKc ->
          withObjCPtr channelNumber $ \raw_channelNumber ->
            withObjCPtr panID $ \raw_panID ->
                sendMsg mtrThreadOperationalDataset (mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:") (retPtr retVoid) [argPtr (castPtr raw_networkName :: Ptr ()), argPtr (castPtr raw_extendedPANID :: Ptr ()), argPtr (castPtr raw_masterKey :: Ptr ()), argPtr (castPtr raw_psKc :: Ptr ()), argPtr (castPtr raw_channelNumber :: Ptr ()), argPtr (castPtr raw_panID :: Ptr ())] >>= ownedObject . castPtr

-- | Create a Thread Operational Dataset object with a RCP formatted active operational dataset. This initializer will return nil if the input data cannot be parsed correctly
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSData data_) => mtrThreadOperationalDataset -> data_ -> IO (Id MTRThreadOperationalDataset)
initWithData mtrThreadOperationalDataset  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg mtrThreadOperationalDataset (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | Get the underlying data that represents the Thread Active Operational Dataset This can be used for the threadOperationalDataset of MTRCommissioningParameters.
--
-- ObjC selector: @- data@
data_ :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
data_ mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSString networkName, IsNSData extendedPANID, IsNSData masterKey, IsNSData psKc, IsNSData panID) => mtrThreadOperationalDataset -> networkName -> extendedPANID -> masterKey -> psKc -> CUShort -> panID -> IO (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID mtrThreadOperationalDataset  networkName extendedPANID masterKey psKc channel panID =
  withObjCPtr networkName $ \raw_networkName ->
    withObjCPtr extendedPANID $ \raw_extendedPANID ->
      withObjCPtr masterKey $ \raw_masterKey ->
        withObjCPtr psKc $ \raw_psKc ->
          withObjCPtr panID $ \raw_panID ->
              sendMsg mtrThreadOperationalDataset (mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:") (retPtr retVoid) [argPtr (castPtr raw_networkName :: Ptr ()), argPtr (castPtr raw_extendedPANID :: Ptr ()), argPtr (castPtr raw_masterKey :: Ptr ()), argPtr (castPtr raw_psKc :: Ptr ()), argCUInt (fromIntegral channel), argPtr (castPtr raw_panID :: Ptr ())] >>= ownedObject . castPtr

-- | The Thread Network name
--
-- ObjC selector: @- networkName@
networkName :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSString)
networkName mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread Network extendended PAN ID
--
-- ObjC selector: @- extendedPANID@
extendedPANID :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
extendedPANID mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "extendedPANID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The 16 byte Master Key
--
-- ObjC selector: @- masterKey@
masterKey :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
masterKey mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "masterKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread PSKc
--
-- ObjC selector: @- PSKc@
psKc :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
psKc mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "PSKc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread network channel.  Always an unsigned 16-bit integer.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSNumber)
channelNumber mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "channelNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A uint16_t stored as 2-bytes in host order representing the Thread PAN ID
--
-- ObjC selector: @- panID@
panID :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
panID mtrThreadOperationalDataset  =
    sendMsg mtrThreadOperationalDataset (mkSelector "panID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- channel@
channel :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO CUShort
channel mtrThreadOperationalDataset  =
    fmap fromIntegral $ sendMsg mtrThreadOperationalDataset (mkSelector "channel") retCUInt []

-- | @- setChannel:@
setChannel :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> CUShort -> IO ()
setChannel mtrThreadOperationalDataset  value =
    sendMsg mtrThreadOperationalDataset (mkSelector "setChannel:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector :: Selector
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector = mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector :: Selector
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector = mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @extendedPANID@
extendedPANIDSelector :: Selector
extendedPANIDSelector = mkSelector "extendedPANID"

-- | @Selector@ for @masterKey@
masterKeySelector :: Selector
masterKeySelector = mkSelector "masterKey"

-- | @Selector@ for @PSKc@
psKcSelector :: Selector
psKcSelector = mkSelector "PSKc"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @panID@
panIDSelector :: Selector
panIDSelector = mkSelector "panID"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

