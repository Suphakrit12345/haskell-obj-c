{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a web application manifest
--
-- Generated bindings for @BEWebAppManifest@.
module ObjC.BrowserEngineKit.BEWebAppManifest
  ( BEWebAppManifest
  , IsBEWebAppManifest(..)
  , init_
  , initWithJSONData_manifestURL
  , jsonData
  , manifestURL
  , initSelector
  , initWithJSONData_manifestURLSelector
  , jsonDataSelector
  , manifestURLSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id BEWebAppManifest)
init_ beWebAppManifest  =
    sendMsg beWebAppManifest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns nil if manifestURL is invalid or jsonData cannot be parsed.
--
-- ObjC selector: @- initWithJSONData:manifestURL:@
initWithJSONData_manifestURL :: (IsBEWebAppManifest beWebAppManifest, IsNSData jsonData, IsNSURL manifestURL) => beWebAppManifest -> jsonData -> manifestURL -> IO (Id BEWebAppManifest)
initWithJSONData_manifestURL beWebAppManifest  jsonData manifestURL =
  withObjCPtr jsonData $ \raw_jsonData ->
    withObjCPtr manifestURL $ \raw_manifestURL ->
        sendMsg beWebAppManifest (mkSelector "initWithJSONData:manifestURL:") (retPtr retVoid) [argPtr (castPtr raw_jsonData :: Ptr ()), argPtr (castPtr raw_manifestURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- jsonData@
jsonData :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id NSData)
jsonData beWebAppManifest  =
    sendMsg beWebAppManifest (mkSelector "jsonData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- manifestURL@
manifestURL :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id NSURL)
manifestURL beWebAppManifest  =
    sendMsg beWebAppManifest (mkSelector "manifestURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithJSONData:manifestURL:@
initWithJSONData_manifestURLSelector :: Selector
initWithJSONData_manifestURLSelector = mkSelector "initWithJSONData:manifestURL:"

-- | @Selector@ for @jsonData@
jsonDataSelector :: Selector
jsonDataSelector = mkSelector "jsonData"

-- | @Selector@ for @manifestURL@
manifestURLSelector :: Selector
manifestURLSelector = mkSelector "manifestURL"

