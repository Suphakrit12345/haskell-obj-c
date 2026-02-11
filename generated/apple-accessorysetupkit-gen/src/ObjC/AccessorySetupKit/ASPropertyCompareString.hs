{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that specifies how to filter a property against a given string and comparison options.
--
-- Generated bindings for @ASPropertyCompareString@.
module ObjC.AccessorySetupKit.ASPropertyCompareString
  ( ASPropertyCompareString
  , IsASPropertyCompareString(..)
  , initWithString_compareOptions
  , init_
  , new
  , string
  , compareOptions
  , initWithString_compareOptionsSelector
  , initSelector
  , newSelector
  , stringSelector
  , compareOptionsSelector

  -- * Enum types
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch

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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a property compare string instance with the given string and comparison options. - Parameters:   - string: The string to compare against.   - compareOptions: Options to apply when comparing strings.
--
-- ObjC selector: @- initWithString:compareOptions:@
initWithString_compareOptions :: (IsASPropertyCompareString asPropertyCompareString, IsNSString string) => asPropertyCompareString -> string -> NSStringCompareOptions -> IO (Id ASPropertyCompareString)
initWithString_compareOptions asPropertyCompareString  string compareOptions =
  withObjCPtr string $ \raw_string ->
      sendMsg asPropertyCompareString (mkSelector "initWithString:compareOptions:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce compareOptions)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id ASPropertyCompareString)
init_ asPropertyCompareString  =
    sendMsg asPropertyCompareString (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id ASPropertyCompareString)
new asPropertyCompareString  =
    sendMsg asPropertyCompareString (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The string to compare against.
--
-- ObjC selector: @- string@
string :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id NSString)
string asPropertyCompareString  =
    sendMsg asPropertyCompareString (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Comparison options to apply when comparing strings.
--
-- ObjC selector: @- compareOptions@
compareOptions :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO NSStringCompareOptions
compareOptions asPropertyCompareString  =
    fmap (coerce :: CULong -> NSStringCompareOptions) $ sendMsg asPropertyCompareString (mkSelector "compareOptions") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithString:compareOptions:@
initWithString_compareOptionsSelector :: Selector
initWithString_compareOptionsSelector = mkSelector "initWithString:compareOptions:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @compareOptions@
compareOptionsSelector :: Selector
compareOptionsSelector = mkSelector "compareOptions"

