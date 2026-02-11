{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ThreadNetwork.Internal.Classes (
    module ObjC.ThreadNetwork.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- THClient ----------

-- | A class that supports safely sharing Thread credentials between multiple clients.
--
-- Request credentials for either a specific Thread network or for the _preferred network_ using @THClient@. The preferred network is the default Thread network chosen by the framework for a home.
--
-- The ThreadNetwork framework maintains a database of network credentials. The class allows clients to store, list, and delete credentials for a given network from the database.
--
-- Some methods in @THClient@ use the _team ID_, a string that you store in your application’s @Info.plist@. The ThreadNetwork framework uses the team ID to preserve the privacy of the Thread network credentials across different clients. For example, credentials stored by one client can’t be deleted or modified by another client.
--
-- - Important: Thread credentials give you the ability to add any device into   the Thread network. Use this information responsibly.
-- 
-- Phantom type for @THClient@.
data THClient

instance IsObjCObject (Id THClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "THClient"

class IsNSObject a => IsTHClient a where
  toTHClient :: a -> Id THClient

instance IsTHClient (Id THClient) where
  toTHClient = unsafeCastId

instance IsNSObject (Id THClient) where
  toNSObject = unsafeCastId

-- ---------- THCredentials ----------

-- | A class that contains credentials for a Thread network.
--
-- A Thread network defines parameters that all connected devices use. ``THCredentials`` provides these parameters.
-- 
-- Phantom type for @THCredentials@.
data THCredentials

instance IsObjCObject (Id THCredentials) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "THCredentials"

class IsNSObject a => IsTHCredentials a where
  toTHCredentials :: a -> Id THCredentials

instance IsTHCredentials (Id THCredentials) where
  toTHCredentials = unsafeCastId

instance IsNSObject (Id THCredentials) where
  toNSObject = unsafeCastId
