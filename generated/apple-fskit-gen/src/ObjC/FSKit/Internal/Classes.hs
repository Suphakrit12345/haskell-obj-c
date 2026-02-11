{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.FSKit.Internal.Classes (
    module ObjC.FSKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- FSClient ----------

-- | An interface for apps and daemons to interact with FSKit.
--
-- FSClient is the primary management interface for FSKit. Use this class to discover FSKit extensions installed on the system, including your own.
--
-- > Important: Don't subclass @FSClient@.
-- 
-- Phantom type for @FSClient@.
data FSClient

instance IsObjCObject (Id FSClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSClient"

class IsNSObject a => IsFSClient a where
  toFSClient :: a -> Id FSClient

instance IsFSClient (Id FSClient) where
  toFSClient = unsafeCastId

instance IsNSObject (Id FSClient) where
  toNSObject = unsafeCastId

-- ---------- FSContainerStatus ----------

-- | A type that represents a container's status.
--
-- This type contains two properties:
--
-- * The ``state`` value that indicates the state of the container, such as ``FSContainerState/ready`` or ``FSContainerState/blocked``. * The ``status`` is an error (optional in Swift, nullable in Objective-C) that provides further information about the state, such as why the container is blocked.
--
-- Examples of statuses that require intervention include errors that indicate the container isn't ready (POSIX @EAGAIN@ or @ENOTCONN@), the container needs authentication (@ENEEDAUTH@), or that authentication failed (@EAUTH@). The status can also be an informative error, such as the FSKit error ``FSError/Code/statusOperationInProgress``.
-- 
-- Phantom type for @FSContainerStatus@.
data FSContainerStatus

instance IsObjCObject (Id FSContainerStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSContainerStatus"

class IsNSObject a => IsFSContainerStatus a where
  toFSContainerStatus :: a -> Id FSContainerStatus

instance IsFSContainerStatus (Id FSContainerStatus) where
  toFSContainerStatus = unsafeCastId

instance IsNSObject (Id FSContainerStatus) where
  toNSObject = unsafeCastId

-- ---------- FSDirectoryEntryPacker ----------

-- | An object used to provide items during a directory enumeration.
--
-- You use this type in your implementation of ``FSVolume/Operations/enumerateDirectory(_:startingAt:verifier:attributes:packer:replyHandler:)``.
--
-- Packing allows your implementation to provide information FSKit needs, including each item's name, type, and identifier (such as an inode number). Some directory enumerations require other attributes, as indicated by the ``FSItemGetAttributesRequest`` sent to the enumerate method.
-- 
-- Phantom type for @FSDirectoryEntryPacker@.
data FSDirectoryEntryPacker

instance IsObjCObject (Id FSDirectoryEntryPacker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSDirectoryEntryPacker"

class IsNSObject a => IsFSDirectoryEntryPacker a where
  toFSDirectoryEntryPacker :: a -> Id FSDirectoryEntryPacker

instance IsFSDirectoryEntryPacker (Id FSDirectoryEntryPacker) where
  toFSDirectoryEntryPacker = unsafeCastId

instance IsNSObject (Id FSDirectoryEntryPacker) where
  toNSObject = unsafeCastId

-- ---------- FSEntityIdentifier ----------

-- | A base type that identifies containers and volumes.
--
-- An ``FSEntityIdentifier`` is a UUID to identify a container or volume, optionally with eight bytes of qualifying (differentiating) data. You use the qualifiers in cases in which a file server can receive multiple connections from the same client, which differ by user credentials. In this case, the identifier for each client is the server's base UUID, and a unique qualifier that differs by client.
--
-- > Important: Don't subclass this class.
-- 
-- Phantom type for @FSEntityIdentifier@.
data FSEntityIdentifier

instance IsObjCObject (Id FSEntityIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSEntityIdentifier"

class IsNSObject a => IsFSEntityIdentifier a where
  toFSEntityIdentifier :: a -> Id FSEntityIdentifier

instance IsFSEntityIdentifier (Id FSEntityIdentifier) where
  toFSEntityIdentifier = unsafeCastId

instance IsNSObject (Id FSEntityIdentifier) where
  toNSObject = unsafeCastId

-- ---------- FSExtentPacker ----------

-- | A type that directs the kernel to map space on disk to a specific file managed by this file system.
--
-- _Extents_ provide the kernel the logical-to-physical mapping of a given file. An extent describes a physical offset on disk, and a length and a logical offset within the file. Rather than working with extents directly, you use this type's methods to provide or "pack" extent information, which FSKit then passes to the kernel.
-- 
-- Phantom type for @FSExtentPacker@.
data FSExtentPacker

instance IsObjCObject (Id FSExtentPacker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSExtentPacker"

class IsNSObject a => IsFSExtentPacker a where
  toFSExtentPacker :: a -> Id FSExtentPacker

instance IsFSExtentPacker (Id FSExtentPacker) where
  toFSExtentPacker = unsafeCastId

instance IsNSObject (Id FSExtentPacker) where
  toNSObject = unsafeCastId

-- ---------- FSFileName ----------

-- | The name of a file, expressed as a data buffer.
--
-- @FSFileName@ is the class that carries filenames from the kernel to @FSModule@ instances, and carries names back to the kernel as part of directory enumeration.
--
-- A filename is usually a valid UTF-8 sequence, but can be an arbitrary byte sequence that doesn't conform to that format. As a result, the ``data`` property always contains a value, but the ``string`` property may be empty. An @FSModule@ can receive an @FSFileName@ that isn't valid UTF-8 in two cases: 1. A program passes erroneous data to a system call. The @FSModule@ treats this situation as an error. 2. An @FSModule@ lacks the character encoding used for a file name. This situation occurs because some file system formats consider a filename to be an arbitrary "bag of bytes," and leave character encoding up to the operating system. Without encoding information, the @FSModule@ can only pass back the names it finds on disk. In this case, the behavior of upper layers such as <doc://com.apple.documentation/documentation/Foundation/NSFileManager> is unspecified. However, the @FSModule@ must support looking up such names and using them as the source name of rename operations. The @FSModule@ must also be able to support filenames that are derivatives of filenames returned from directory enumeration. Derivative filenames include Apple Double filenames (@"._Name"@), and editor backup filenames.
--
-- > Important: Don't subclass this class.
-- 
-- Phantom type for @FSFileName@.
data FSFileName

instance IsObjCObject (Id FSFileName) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSFileName"

class IsNSObject a => IsFSFileName a where
  toFSFileName :: a -> Id FSFileName

instance IsFSFileName (Id FSFileName) where
  toFSFileName = unsafeCastId

instance IsNSObject (Id FSFileName) where
  toNSObject = unsafeCastId

-- ---------- FSFileSystem ----------

-- | An abstract base class for implementing a full-featured file system.
--
-- @FSFileSystem@ is a full-featured file system, which works with one or more ``FSResource`` instances and presents one or more ``FSVolume`` references to callers.
--
-- Implement your app extension by providing a subclass of @FSFileSystem@ as a delegate object. Your delegate also needs to implement the @FSFileSystemOperations@ protocol so that it can probe, load, and unload resources.
--
-- > Note: The current version of FSKit supports only ``FSUnaryFileSystem``, not @FSFileSystem@.
-- 
-- Phantom type for @FSFileSystem@.
data FSFileSystem

instance IsObjCObject (Id FSFileSystem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSFileSystem"

class IsNSObject a => IsFSFileSystem a where
  toFSFileSystem :: a -> Id FSFileSystem

instance IsFSFileSystem (Id FSFileSystem) where
  toFSFileSystem = unsafeCastId

instance IsNSObject (Id FSFileSystem) where
  toNSObject = unsafeCastId

-- ---------- FSItem ----------

-- | A distinct object in a file hierarchy, such as a file, directory, symlink, socket, and more.
--
-- An @FSItem@ is a mostly opaque object, which your file system implementation defines as needed.
--
-- The ``FSItemAttributes`` class defines nonatomic properties to support @FSItem@ instances. An ``FSItemAttributes`` instance contains a snapshot of the attributes of an @FSItem@ at one point in time. The ``FSItemAttributes`` properties have no explicit thread safety provisions, since the operations that either get or set these properties enforce thread safety.
--
-- You test an attribute's validity with the the method ``FSItem/Attributes/isValid(_:)``. If the value is @true@ (Swift) or @YES@ (Objective-C), it's safe to use the attribute.
--
-- Methods that get or set an item's attribute use ``FSItemGetAttributesRequest`` or ``FSItemSetAttributesRequest``, respectively. Both are subclasses of ``FSItemAttributes``. An ``FSItemGetAttributesRequest`` contains a ``FSItemGetAttributesRequest/wantedAttributes`` property to indicate the attributes a file system provides for the request. Similarly, ``FSItemSetAttributesRequest`` uses the property ``FSItemSetAttributesRequest/consumedAttributes`` for a file system to signal back which attributes it successfully used.
--
-- @FSItem@ is the FSKit equivelant of a vnode in the kernel. For every FSKit vnode in the kernel, the @FSModule@ hosting the volume has an instantiated @FSItem@.
-- 
-- Phantom type for @FSItem@.
data FSItem

instance IsObjCObject (Id FSItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSItem"

class IsNSObject a => IsFSItem a where
  toFSItem :: a -> Id FSItem

instance IsFSItem (Id FSItem) where
  toFSItem = unsafeCastId

instance IsNSObject (Id FSItem) where
  toNSObject = unsafeCastId

-- ---------- FSItemAttributes ----------

-- | Attributes of an item, such as size, creation and modification times, and user and group identifiers.
-- 
-- Phantom type for @FSItemAttributes@.
data FSItemAttributes

instance IsObjCObject (Id FSItemAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSItemAttributes"

class IsNSObject a => IsFSItemAttributes a where
  toFSItemAttributes :: a -> Id FSItemAttributes

instance IsFSItemAttributes (Id FSItemAttributes) where
  toFSItemAttributes = unsafeCastId

instance IsNSObject (Id FSItemAttributes) where
  toNSObject = unsafeCastId

-- ---------- FSItemGetAttributesRequest ----------

-- | A request to get attributes from an item.
--
-- Methods that retrieve attributes use this type and inspect the ``wantedAttributes`` property to determine which attributes to provide. FSKit calls the ``isAttributeWanted(_:)`` method to determine whether the request requires a given attribute.
-- 
-- Phantom type for @FSItemGetAttributesRequest@.
data FSItemGetAttributesRequest

instance IsObjCObject (Id FSItemGetAttributesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSItemGetAttributesRequest"

class IsNSObject a => IsFSItemGetAttributesRequest a where
  toFSItemGetAttributesRequest :: a -> Id FSItemGetAttributesRequest

instance IsFSItemGetAttributesRequest (Id FSItemGetAttributesRequest) where
  toFSItemGetAttributesRequest = unsafeCastId

instance IsNSObject (Id FSItemGetAttributesRequest) where
  toNSObject = unsafeCastId

-- ---------- FSMetadataRange ----------

-- | A range that describes contiguous metadata segments on disk.
--
-- This type represents a range that begins at @startOffset@ and ends at @startOffset + segmentLength * segmentCount@. Each segment in the range represents a single block in the resource's buffer cache.
--
-- For example, given an @FSMetadataRange@ with the following properties:
--
-- * @startOffset = 0@ * @segmentLength = 512@ * @segmentCount = 8@
--
-- The range represents eight segments: from 0 to 511, then from 512 to 1023, and so on until a final segment of 3584 to 4095.
--
-- Ensure that each metadata segment represents a range that's already present in the resource's buffer cache. Similarly, ensure that each segment's offset and length matches the offset and length of the corresponding block in the buffer cache.
-- 
-- Phantom type for @FSMetadataRange@.
data FSMetadataRange

instance IsObjCObject (Id FSMetadataRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSMetadataRange"

class IsNSObject a => IsFSMetadataRange a where
  toFSMetadataRange :: a -> Id FSMetadataRange

instance IsFSMetadataRange (Id FSMetadataRange) where
  toFSMetadataRange = unsafeCastId

instance IsNSObject (Id FSMetadataRange) where
  toNSObject = unsafeCastId

-- ---------- FSModuleIdentity ----------

-- | An installed file system module.
-- 
-- Phantom type for @FSModuleIdentity@.
data FSModuleIdentity

instance IsObjCObject (Id FSModuleIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSModuleIdentity"

class IsNSObject a => IsFSModuleIdentity a where
  toFSModuleIdentity :: a -> Id FSModuleIdentity

instance IsFSModuleIdentity (Id FSModuleIdentity) where
  toFSModuleIdentity = unsafeCastId

instance IsNSObject (Id FSModuleIdentity) where
  toNSObject = unsafeCastId

-- ---------- FSMutableFileDataBuffer ----------

-- | A wrapper object for a data buffer.
--
-- This object provides a "zero-copy" buffer, for use when reading data from files. By not requiring additional buffer copying, this object reduces the extension's memory footprint and improves performance. The @FSMutableFileDataBuffer@ behaves similarly to a @uio@ in the kernel.
-- 
-- Phantom type for @FSMutableFileDataBuffer@.
data FSMutableFileDataBuffer

instance IsObjCObject (Id FSMutableFileDataBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSMutableFileDataBuffer"

class IsNSObject a => IsFSMutableFileDataBuffer a where
  toFSMutableFileDataBuffer :: a -> Id FSMutableFileDataBuffer

instance IsFSMutableFileDataBuffer (Id FSMutableFileDataBuffer) where
  toFSMutableFileDataBuffer = unsafeCastId

instance IsNSObject (Id FSMutableFileDataBuffer) where
  toNSObject = unsafeCastId

-- ---------- FSProbeResult ----------

-- | An object that represents the results of a specific probe.
--
-- For any ``result`` value other than ``FSMatchResult/notRecognized``, ensure the ``name`` and ``containerID`` values are non-@nil@. When a container or volume format doesn't use a name, return an empty string. Also use an empty string in the case in which the format supports a name, but the value isn't set yet.
--
-- Some container or volume formats may lack a durable UUID on which to base a container identifier. This situation is only valid for unary file systems. In such a case, return a random UUID.
--
-- With a block device resource, a probe operation may successfully get a result but encounter an error reading the name or UUID. If this happens, use whatever information is available, and provide an empty string or random UUID for the name or container ID, respectively.
-- 
-- Phantom type for @FSProbeResult@.
data FSProbeResult

instance IsObjCObject (Id FSProbeResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSProbeResult"

class IsNSObject a => IsFSProbeResult a where
  toFSProbeResult :: a -> Id FSProbeResult

instance IsFSProbeResult (Id FSProbeResult) where
  toFSProbeResult = unsafeCastId

instance IsNSObject (Id FSProbeResult) where
  toNSObject = unsafeCastId

-- ---------- FSResource ----------

-- | An abstract resource a file system uses to provide data for a volume.
--
-- @FSResource@ is a base class to represent the various possible sources of data for a file system. These range from dedicated storage devices like hard drives and flash storage to network connections, and beyond. Subclasses define behavior specific to a given kind of resource, such as ``FSBlockDeviceResource-c.class`` for disk partition (IOMedia) file systems. These file systems are typical disk file systems such as HFS, APFS, ExFAT, ext2fs, or NTFS.
--
-- A resource's type also determines its life cycle. Resources based on block storage devices come into being when the system probes the media underlying the volumes and container. Other kinds of resources, like those based on URLs, might have different life cycles. For example, a resource based on a @file://@ URL might iniitalize when a person uses the "Connect to server" command in the macOS Finder.
--
-- ### Proxying resources
--
-- Some resources, like ``FSBlockDeviceResource``, come in proxy and non-proxy variants. This addresses the issue that opening an external device like @/dev/disk2s1@ requires an entitlement. Proxy resources allow unentitled clients of FSKit to describe which disk an ``FSBlockDeviceResource`` should represent. This allows, for example, the @mount(8)@ tool to mount FSKit file systems on block devices when run as root. The tool uses a proxy when executing a command like @mount -t ffs /dev/disk2s1 /some/path@, which prevents leaking privileged resource access.
-- 
-- Phantom type for @FSResource@.
data FSResource

instance IsObjCObject (Id FSResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSResource"

class IsNSObject a => IsFSResource a where
  toFSResource :: a -> Id FSResource

instance IsFSResource (Id FSResource) where
  toFSResource = unsafeCastId

instance IsNSObject (Id FSResource) where
  toNSObject = unsafeCastId

-- ---------- FSStatFSResult ----------

-- | A type used to report a volume's statistics.
--
-- The names of this type's properties match those in the @statfs@ structure in @statfs(2)@, which reports these values for an FSKit file system. All numeric properties default to @0@. Override these values, unless a given property has no meaningful value to provide.
--
-- > Note: Available space, free space, total space, and used space have properties to express their values either as a number of blocks or a number of bytes. Your module may supply both of these values by setting both the relevant block or byte property. Alternatively, a module may set only one of the two properties. When you do this, FSKit calculates the matching value based on ``blockSize``.
--
-- For the read-only ``fileSystemTypeName``, set this value with the designated initializer.
-- 
-- Phantom type for @FSStatFSResult@.
data FSStatFSResult

instance IsObjCObject (Id FSStatFSResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSStatFSResult"

class IsNSObject a => IsFSStatFSResult a where
  toFSStatFSResult :: a -> Id FSStatFSResult

instance IsFSStatFSResult (Id FSStatFSResult) where
  toFSStatFSResult = unsafeCastId

instance IsNSObject (Id FSStatFSResult) where
  toNSObject = unsafeCastId

-- ---------- FSTask ----------

-- | A class that enables a file system module to pass log messages and completion notifications to clients.
--
-- FSKit creates an instance of this class for each long-running operations.
-- 
-- Phantom type for @FSTask@.
data FSTask

instance IsObjCObject (Id FSTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSTask"

class IsNSObject a => IsFSTask a where
  toFSTask :: a -> Id FSTask

instance IsFSTask (Id FSTask) where
  toFSTask = unsafeCastId

instance IsNSObject (Id FSTask) where
  toNSObject = unsafeCastId

-- ---------- FSTaskOptions ----------

-- | A class that passes command options to a task, optionally providing security-scoped URLs.
-- 
-- Phantom type for @FSTaskOptions@.
data FSTaskOptions

instance IsObjCObject (Id FSTaskOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSTaskOptions"

class IsNSObject a => IsFSTaskOptions a where
  toFSTaskOptions :: a -> Id FSTaskOptions

instance IsFSTaskOptions (Id FSTaskOptions) where
  toFSTaskOptions = unsafeCastId

instance IsNSObject (Id FSTaskOptions) where
  toNSObject = unsafeCastId

-- ---------- FSUnaryFileSystem ----------

-- | An abstract base class for implementing a minimal file system.
--
-- @FSUnaryFileSystem@ is a simplified file system, which works with one ``FSResource`` and presents it as one ``FSVolume``.
--
-- The one volume and its container have a shared state and lifetime, a more constrained life cycle than the ``FSFileSystem`` design flow.
--
-- Implement your app extension by providing a subclass of @FSUnaryFileSystem@ as a delegate object. Your delegate also needs to implement the ``FSUnaryFileSystemOperations`` protocol so that it can load resources.
-- 
-- Phantom type for @FSUnaryFileSystem@.
data FSUnaryFileSystem

instance IsObjCObject (Id FSUnaryFileSystem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSUnaryFileSystem"

class IsNSObject a => IsFSUnaryFileSystem a where
  toFSUnaryFileSystem :: a -> Id FSUnaryFileSystem

instance IsFSUnaryFileSystem (Id FSUnaryFileSystem) where
  toFSUnaryFileSystem = unsafeCastId

instance IsNSObject (Id FSUnaryFileSystem) where
  toNSObject = unsafeCastId

-- ---------- FSVolume ----------

-- | A directory structure for files and folders.
--
-- A file system, depending on its type, provides one or more volumes to clients. The ``FSUnaryFileSystem`` by definition provides only one volume, while an ``FSFileSystem`` supports multiple volumes.
--
-- You implement a volume for your file system type by subclassing this class, and also conforming to the ``FSVolume/Operations`` and ``FSVolume/PathConfOperations`` protocols. This protocol defines the minimum set of operations supported by a volume, such as mounting, activating, creating and removing items, and more.
--
-- Your volume can provide additional functionality by conforming to other volume operations protocols. These protocols add support for operations like open and close, read and write, extended attribute (Xattr) manipulation, and more.
-- 
-- Phantom type for @FSVolume@.
data FSVolume

instance IsObjCObject (Id FSVolume) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSVolume"

class IsNSObject a => IsFSVolume a where
  toFSVolume :: a -> Id FSVolume

instance IsFSVolume (Id FSVolume) where
  toFSVolume = unsafeCastId

instance IsNSObject (Id FSVolume) where
  toNSObject = unsafeCastId

-- ---------- FSVolumeSupportedCapabilities ----------

-- | A type that represents capabillities supported by a volume, such as hard and symbolic links, journaling, and large file sizes.
-- 
-- Phantom type for @FSVolumeSupportedCapabilities@.
data FSVolumeSupportedCapabilities

instance IsObjCObject (Id FSVolumeSupportedCapabilities) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSVolumeSupportedCapabilities"

class IsNSObject a => IsFSVolumeSupportedCapabilities a where
  toFSVolumeSupportedCapabilities :: a -> Id FSVolumeSupportedCapabilities

instance IsFSVolumeSupportedCapabilities (Id FSVolumeSupportedCapabilities) where
  toFSVolumeSupportedCapabilities = unsafeCastId

instance IsNSObject (Id FSVolumeSupportedCapabilities) where
  toNSObject = unsafeCastId

-- ---------- FSContainerIdentifier ----------

-- | A type that identifies a container.
--
-- The identifier is either a UUID or a UUID with additional differentiating bytes. Some network protocols evaluate access based on a user ID when connecting. In this situation, when a file server receives multiple client connections with different user IDs, the server provides different file hierarchies to each. For such systems, represent the container identifier as the UUID associated with the server, followed by four or eight bytes to differentiate connections.
--
-- > Important: Don't subclass this class.
-- 
-- Phantom type for @FSContainerIdentifier@.
data FSContainerIdentifier

instance IsObjCObject (Id FSContainerIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSContainerIdentifier"

class IsFSEntityIdentifier a => IsFSContainerIdentifier a where
  toFSContainerIdentifier :: a -> Id FSContainerIdentifier

instance IsFSContainerIdentifier (Id FSContainerIdentifier) where
  toFSContainerIdentifier = unsafeCastId

instance IsFSEntityIdentifier (Id FSContainerIdentifier) where
  toFSEntityIdentifier = unsafeCastId

instance IsNSObject (Id FSContainerIdentifier) where
  toNSObject = unsafeCastId

-- ---------- FSVolumeIdentifier ----------

-- | A type that identifies a volume.
--
-- For most volumes, the volume identifier is the UUID identifying the volume.
--
-- Network file systems may access the same underlying volume using different authentication credentials. To handle this situation, add qualifying data to identify the specific container, as discussed in the superclass, ``FSEntityIdentifier``.
--
-- > Important: Don't subclass this class.
-- 
-- Phantom type for @FSVolumeIdentifier@.
data FSVolumeIdentifier

instance IsObjCObject (Id FSVolumeIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSVolumeIdentifier"

class IsFSEntityIdentifier a => IsFSVolumeIdentifier a where
  toFSVolumeIdentifier :: a -> Id FSVolumeIdentifier

instance IsFSVolumeIdentifier (Id FSVolumeIdentifier) where
  toFSVolumeIdentifier = unsafeCastId

instance IsFSEntityIdentifier (Id FSVolumeIdentifier) where
  toFSEntityIdentifier = unsafeCastId

instance IsNSObject (Id FSVolumeIdentifier) where
  toNSObject = unsafeCastId

-- ---------- FSItemSetAttributesRequest ----------

-- | A request to set attributes on an item.
--
-- Methods that take attributes use this type to receive attribute values and to indicate which attributes they support. The various members of the parent type, ``FSItemAttributes``, contain the values of the attributes to set.
--
-- Modify the ``consumedAttributes`` property to indicate which attributes your file system successfully used. FSKit calls the ``wasAttributeConsumed(_:)`` method to determine whether the file system successfully used a given attribute. Only set the attributes that your file system supports.
-- 
-- Phantom type for @FSItemSetAttributesRequest@.
data FSItemSetAttributesRequest

instance IsObjCObject (Id FSItemSetAttributesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSItemSetAttributesRequest"

class IsFSItemAttributes a => IsFSItemSetAttributesRequest a where
  toFSItemSetAttributesRequest :: a -> Id FSItemSetAttributesRequest

instance IsFSItemSetAttributesRequest (Id FSItemSetAttributesRequest) where
  toFSItemSetAttributesRequest = unsafeCastId

instance IsFSItemAttributes (Id FSItemSetAttributesRequest) where
  toFSItemAttributes = unsafeCastId

instance IsNSObject (Id FSItemSetAttributesRequest) where
  toNSObject = unsafeCastId

-- ---------- FSBlockDeviceResource ----------

-- | A resource that represents a block storage disk partition.
--
-- A @FSBlockDeviceResource@ can exist in either a proxied or nonproxied version.  Only the @fskitd@ daemon creates "real" (nonproxied) instances of this class.  Client applications and daemons create proxy objects for requests, and @fskitd@ opens the underlying device during the processing of the request.
--
-- This class wraps a file descriptor for a disk device or partition.  Its fundamental identifier is the BSD disk name (``bsdName``) for the underlying IOMedia object.  However, ``FSBlockDeviceResource-c.class`` doesn't expose the underlying file descriptor.  Instead, it provides accessor methods that can read from and write to the partition, either directly or using the kernel buffer cache.
--
-- When you use a @FSBlockDeviceResource@, your file system implementation also conforms to a maintenance operation protocol.  These protocols add support for checking, repairing, and optionally formatting file systems.  The system doesn't mount block device file systems until they pass a file system check.  For an ``FSUnaryFileSystem`` that uses @FSBlockDeviceResource@, conform to @FSManageableResourceMaintenanceOperations@.
-- 
-- Phantom type for @FSBlockDeviceResource@.
data FSBlockDeviceResource

instance IsObjCObject (Id FSBlockDeviceResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSBlockDeviceResource"

class IsFSResource a => IsFSBlockDeviceResource a where
  toFSBlockDeviceResource :: a -> Id FSBlockDeviceResource

instance IsFSBlockDeviceResource (Id FSBlockDeviceResource) where
  toFSBlockDeviceResource = unsafeCastId

instance IsFSResource (Id FSBlockDeviceResource) where
  toFSResource = unsafeCastId

instance IsNSObject (Id FSBlockDeviceResource) where
  toNSObject = unsafeCastId

-- ---------- FSGenericURLResource ----------

-- | A resource that represents an abstract URL.
--
-- An @FSGenericURLResource@ is a completely abstract resource. The only reference to its contents is a single URL, the contents of which are arbitrary. This URL might represent a PCI locator string like `/pci/usb\@5`, or some sort of network address for a remote file system. FSKit leaves interpretation of the URL and its contents entirely up to your implementation.
--
-- Use the @Info.plist@ key @FSSupportedSchemes@ to provide an array of case-insensitive URL schemes that your implementation supports. The following example shows how a hypothetical @FSGenericURLResource@ implementation declares support for the @rsh@ and @ssh@ URL schemes: ``` <key>FSSupportedSchemes</key> <array>     <string>rsh</string>     <string>ssh</string> </array> ```
-- 
-- Phantom type for @FSGenericURLResource@.
data FSGenericURLResource

instance IsObjCObject (Id FSGenericURLResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSGenericURLResource"

class IsFSResource a => IsFSGenericURLResource a where
  toFSGenericURLResource :: a -> Id FSGenericURLResource

instance IsFSGenericURLResource (Id FSGenericURLResource) where
  toFSGenericURLResource = unsafeCastId

instance IsFSResource (Id FSGenericURLResource) where
  toFSResource = unsafeCastId

instance IsNSObject (Id FSGenericURLResource) where
  toNSObject = unsafeCastId

-- ---------- FSPathURLResource ----------

-- | A resource that represents a path in the system file space.
--
-- The URL passed to @FSPathURLResource@ may be a security-scoped URL. If the URL is a security-scoped URL, FSKit transports it intact from a client application to your extension.
-- 
-- Phantom type for @FSPathURLResource@.
data FSPathURLResource

instance IsObjCObject (Id FSPathURLResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FSPathURLResource"

class IsFSResource a => IsFSPathURLResource a where
  toFSPathURLResource :: a -> Id FSPathURLResource

instance IsFSPathURLResource (Id FSPathURLResource) where
  toFSPathURLResource = unsafeCastId

instance IsFSResource (Id FSPathURLResource) where
  toFSResource = unsafeCastId

instance IsNSObject (Id FSPathURLResource) where
  toNSObject = unsafeCastId
