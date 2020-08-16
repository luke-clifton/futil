module Futil where

import Data.ByteString (ByteString)
import Pipes.Safe
import Pipes as P
import Pipes.Group


-- | A @`Raw`@ is a raw stream of bytes. Any byte can appear in a raw stream.
type Raw  = Raw' (SafeT IO)
type Raw' = Producer ByteString

-- | An @`Object`@ is a stream of bytes that does not include the @\0@ byte.
type Object  = Object' (SafeT IO)
type Object' = P.Producer ByteString

-- | A @`Many`@ is a stream of @`Objects`@s delimited by @\0@ bytes.
type Many = Many' (SafeT IO)
type Many' m = FreeT (Object' m) m

newtype FutilCmd a = FutilCmd {unFutil :: a}
type Futile = FutilCmd (Raw () -> Raw ())

class Futil a where
    cmd :: [ByteString] -> a -> FutilCmd (Raw () -> Raw ())
