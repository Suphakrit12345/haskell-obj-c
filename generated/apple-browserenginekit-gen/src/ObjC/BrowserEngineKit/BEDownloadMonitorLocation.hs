{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BEDownloadMonitorLocation@.
module ObjC.BrowserEngineKit.BEDownloadMonitorLocation
  ( BEDownloadMonitorLocation
  , IsBEDownloadMonitorLocation(..)
  , init_
  , new
  , url
  , bookmarkData
  , initSelector
  , newSelector
  , urlSelector
  , bookmarkDataSelector


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
init_ :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id BEDownloadMonitorLocation)
init_ beDownloadMonitorLocation  =
    sendMsg beDownloadMonitorLocation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BEDownloadMonitorLocation)
new  =
  do
    cls' <- getRequiredClass "BEDownloadMonitorLocation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- url@
url :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id NSURL)
url beDownloadMonitorLocation  =
    sendMsg beDownloadMonitorLocation (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bookmarkData@
bookmarkData :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id NSData)
bookmarkData beDownloadMonitorLocation  =
    sendMsg beDownloadMonitorLocation (mkSelector "bookmarkData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @bookmarkData@
bookmarkDataSelector :: Selector
bookmarkDataSelector = mkSelector "bookmarkData"

