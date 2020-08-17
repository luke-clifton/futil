{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futil where

import Lens.Micro
import System.IO
import qualified Data.ByteString.Lazy as LB
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Control.Monad
import Pipes.Safe
import Pipes as P
import Pipes.ByteString as P
import Pipes.Group as PG


-- | A @`Raw`@ is a raw stream of bytes. Any byte can appear in a raw stream.
type Raw  = Raw' (SafeT IO)
type Raw' = Producer ByteString

-- | An @`Object`@ is a stream of bytes that does not include the @\0@ byte.
type Object  = Object' (SafeT IO)
newtype Object' m x = Object {_objectToRaw :: P.Producer ByteString m x}
    deriving newtype (Functor, MonadTrans, Monad, Applicative)

objectToRaw :: Object' m x -> Raw' m x
objectToRaw = _objectToRaw

byteStringToObject :: ByteString -> Object ()
byteStringToObject = validate . P.yield

-- | Runtime check to validate that a @`Raw`@ input is infact an @`Object`@.
-- TODO: Which is better?
validate :: Functor m => Raw' m x -> Object' m x
validate p = Object $ P.for p $ \b -> do
    when (ByteString.elem 0 b) $ fail "Invalid object. Contains '\\0' byte."
    P.yield b

-- This version converts to a @`Many`@, and then ensures there is only one.
-- This allows for a @\\0@ terminated object to be accepted as a SI (as long
-- as there are no extra bytes).
-- validate :: Monad m => Raw' m x -> Object' m x
-- validate p = do
--     PG.lift (runFreeT (toObjects p)) >>= \case
--         Pure x -> pure x
--         Free x -> do
--             p <- x
--             PG.lift (runFreeT p) >>= \case
--                 Pure x -> pure x
--                 Free x -> fail "Expected a single object" -- TODO: provide context

-- | A @`Many`@ is a stream of @`Objects`@s delimited by @\0@ bytes.
type Many = Many' (SafeT IO)
type Many' m = FreeT (Object' m) m

-- | Turn a stream of @`Many`@ objects into raw output. You may want to
-- consider using @`emitObjects`@ instead, which flushes at appropriate points.
output :: Monad m => Many' m x -> Raw' m x
output f = do
    PG.lift (runFreeT f) >>= \case
        Pure x -> pure x
        Free x -> do
            p <- objectToRaw x
            P.yield (ByteString.singleton 0)
            output p


newtype FutilCmd a = FutilCmd {unFutil :: a}
type Futile = FutilCmd (Raw () -> Raw ())

class Futil a where
    cmd :: [ByteString] -> a -> FutilExe

data FutilExe = FutilExe
    { exeO :: Handle -> Raw () -> IO ()
    , exeRaw :: Raw () -> Raw ()
--    , exeI :: Handle -> Raw ()
    }
