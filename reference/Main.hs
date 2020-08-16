{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Discover
import Futil

--      S     M    R   N
-- S  SISO  SIMO SIRO
-- M  MISO  MIMO MIRO
-- R  RISO  RIMO RIRO
-- N
--
-- S -> M / liftF (exactly 1) or toObjects (1 or 0) -- will use toObjects generally, as that is compatible with R. Use `singleton` command to manually enforce.
-- S -> R / id
--
-- M -> R / output
-- M -> S / error "Incompatible" -- M will always produce a \0 byte.
--
-- R -> S / validate -- Will produce a runtime error if invalid
-- R -> M / toObjects
--
-- TODO: How far can we get with rewrite rules to remove output . toObjects?
--
-- Structure.
--
--  1: input | cmd | output
--  2: input | cmd output
--  3: cmd input | output
--  4: input | cmd continuation | output
--
-- Option 2 is particularly useful for command fusion (though, perhaps only
-- really in the map/bind/zip programs).
--
-- Option 3 is useful when used as arguments to map/bind/zip programs.
--
-- For example,
--
--   input | lines map words length
--
-- Makes use of (2) for breaking the input into lines, and mapping the
-- `words length` command on each line, which itself makes use of the
-- (2) option for breaking up the input lines into words and feeding that
-- to the `length` command. Note that the `lines` could have been replaced
-- by a shell pipe, but that words could not.
--
--   input | lines | map words length
--
--   input | lines | map (words | length)  # illegal
--
-- What are some useful examples of (3)
--
--   length lines cat ./filename.txt
--
-- instead of
--
--   cat ./filename.txt | lines length
--
-- We can just provide a "readFile" command that accepts the continuation.
--
--  (2) readFile ./filename.txt lines length
--  (3) length lines readFile ./filename.txt
--
-- Can we always convert between (2) and (3).
--
-- Is (3) more intuitive (and thus, more forwards?)
--
--   lines | map length words
--
-- `map` and friends can't provide a continuation, and thus must always
-- rely on pipes. You always provide map with the "command to run on each
-- element" which makes it more similar to (2) than (3). Perhaps consistency
-- here is more important? (the continuation is run multiple times).
--
--  cons a cons b nil  -- cons (el) ouput -- good use of (2) style.
--                        (output el, exec into continuation)
--  cons a cons b nil  -- cons (el) input -- good use of (3) style.
--                        (output el, call transfer input to output)
--
-- Transformation functions could either come with two versions.
--
--  lines  : input | cmd output 
--  lines' : cmd input | output
--
-- Or, transformation functions could specify it.
--
--  input | lines | output
--  input | lines '>' output
--  lines '<' input | output
--
-- pipeline length '|' words '|' stuff
--
--

import Text.Printf
import Data.Void
import System.Exit
import Safe
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Control.Concurrent.Async
import Data.List
import System.Environment as Env
import Debug.Trace
import Control.Monad.IO.Class
import Lens.Micro
import Data.Word
import GHC.Stack
import Data.List as List
import System.Posix.Env.ByteString
import System.Environment (getProgName)
import Control.Monad
import Shh
import Shh.Internal
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe
import System.IO
import Control.Monad.Trans.Free
import Pipes (Producer, (>->))
import qualified Pipes as P
import qualified Pipes.Group as PG
import qualified Pipes.Prelude as Pr
import qualified Pipes.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as SC8
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import System.Process
import Data.Char
import Pipes.Safe
import Options.Applicative
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M

type ArgParse = ExceptT String (WriterT [String] (State [LB.ByteString]))

test :: Monad m => Producer ByteString m ()
test = P.fromLazy "abcdefghijkl\0b\0"

-- | A lens that decodes a raw stream of bytes in a stream of objects.
objects :: Lens' (Raw x) (Many x)
objects = objects'

objects' :: Monad m => Lens' (Raw' m x) (Many' m x)
objects' k p = fmap (output) (k (toObjects p))

-- | Parse a @`Raw`@ stream of bytes into a stream of @`Object`@s
toObjects :: Monad m => Raw' m x -> Many' m x
toObjects p0 = PG.FreeT (go0 p0)
  where
    predicate = (== 0)
    go0 p = do
        x <- P.next p
        case x of
            Left   r       -> return (PG.Pure r)
            Right (bs, p') ->
                if (ByteString.null bs)
                then go0 p'
                else go1 (P.yield bs >> p')
    go1 p = return $ PG.Free $ do
        p' <- p ^. P.break predicate
        return $ PG.FreeT $ do
            x <- P.nextByte p'
            case x of
                Left   r       -> return (PG.Pure r)
                Right (0, p'') -> do
                    x' <- P.nextByte p''
                    case x' of
                        Left r -> return (PG.Pure r)
                        Right (b, p''') -> go1 (P.yield (ByteString.singleton b) >> p''')

-- Emits objects one by one, flushing after each one. This lends itself well
-- to real-time streaming, but may impact performance.
emitObjects :: Handle -> Many x -> SafeT IO x
emitObjects h input = do
    c <- runFreeT input
    case c of
        Pure x -> pure x
        Free x -> do
            n <- P.runEffect $ (x <* P.yield (ByteString.singleton 0)) >-> P.toHandle h
            liftIO $ hFlush h
            emitObjects h n

-- | Slurp a whole @`Object`@ into memory.
slurp :: Object () -> SafeT IO LB.ByteString
slurp = P.toLazyM

-- | Turn a stream of @`Many`@ objects into raw output. You may want to
-- consider using @`emitObjects`@ instead, which flushes at appropriate points.
output :: Monad m => Many' m x -> Raw' m x
output f = do
    PG.lift (runFreeT f) >>= \case
        Pure x -> pure x
        Free x -> do
            p <- x
            P.yield (ByteString.singleton 0)
            output p

-- | Runtime check to validate that a @`Raw`@ input is infact an @`Object`@.

-- TODO: Which is better?
validate :: Functor m => Raw' m x -> Object' m x
validate p = P.for p $ \b -> do
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

-- | This is a helper class for peeling off @`Object`@s from a stream of
-- @`Many`@, primarily used for feeding as arguments to a function.
class Lambda f  where
    -- Peel off arguments from the @`Many`@, and produce an @`Object`@ returning
    -- the remaining part of the stream.
    apply :: f -> Many x -> Object (Many x)

-- | Slurp a ByteString off, and apply it to the function.
instance Lambda c => Lambda (LB.ByteString -> c) where
    apply f input = do
        c <- PG.lift $ runFreeT input
        case c of
            Pure x -> error "We expected more arguments"
            Free x -> do
                (arg, input') <- PG.lift $ P.toLazyM' x
                let r = f arg
                Main.apply r input'

-- Produce an @`Object`@
instance (m ~ SafeT IO) => Lambda (Object' m x) where
    apply f input = fmap (const input) f

-- | Perform a transformation on each @`Object`@ in a many. The object must
-- prduce its any un-consumed input as a result, which will then be
-- discarded.
mapObjects :: (forall a. Object a -> Object (Object a)) -> Many r -> Many r
mapObjects f t = do
    c <- PG.lift (runFreeT t)
    case c of
        Pure r -> pure r
        Free x -> do
            let a = f x
            x <- liftF a
            r <- PG.lift (P.runEffect $ x >-> Pr.drain)
            mapObjects f r

futil_length :: FutilCmd (Many x -> Object x)
futil_length = FutilCmd $ \inp -> do
    (r, x) <- lift $ cmd_length inp
    P.yield (SC8.pack . show $ r)
    pure x

cmd_length :: Many x -> SafeT IO (Int, x)
cmd_length f = do
    runFreeT f >>= \case
        Pure x -> pure (0, x)
        Free x -> do
            f' <- P.runEffect $ x >-> Pr.drain
            (l,x) <- cmd_length f'
            pure $ (l + 1, x)


-- | Apply a function (of one or more arguments) to a stream of @`Many`@
-- @`Object`@s. Each argument is sourced from an object in the stream.
cmd_mapping :: Lambda l => (LB.ByteString -> l) -> Many r -> Many r
cmd_mapping f input = do
    -- Check if there is another object, if so, we apply, otherwise we stop.
    -- `apply` will pull of as many objects as there are arguments.
    c <- PG.lift (runFreeT input)
    case c of
        Pure r -> pure r
        Free x -> do
            (arg, input') <- PG.lift $ P.toLazyM' x
            input'' <- liftF $ Main.apply (f arg) input'
            cmd_mapping f input'


-- TODO: Add Futil instance for `Lam -> a`
futil_mapping :: [ByteString] -> FutilCmd (Many r -> Many r)
futil_mapping bs = FutilCmd $ \inp -> do
    let Right lam = lamFromList (map LB.fromStrict bs)
    cmd_mapping' lam inp

-- | Similar to @`cmd_mapping`@, except that the @`Lam`@ type is used to
-- represent a "function" that can be constructed at run time.
cmd_mapping' :: Lam -> Many r -> Many r
cmd_mapping' (Lam (c,f)) input = do
    -- Check if there is another object, if so, we apply, otherwise we stop.
    -- `apply` will pull of as many objects as there are arguments.
    (r,bs) <- slurpNatObjs c [] input
    if null bs
    then r
    else do
        let
            (a:ff) = do
                c <- f
                case c of
                    Left b -> pure b
                    Right ix -> pure $ bs !! ix
            FutilCmd cc = cmd (map LB.toStrict ff) (LB.toStrict a)
        --case cc of
        --    Right (Right rc) -> do
        --        liftF $ void $ cmdSysSiso rc (mempty :: Raw ())
        --    Right (Left (CSS (SISO f))) -> do
        --        liftF $ f (mempty :: Object ())
        --    Right (Left _) -> fail "Incorrect type"
        --    _ -> undefined -- TODO: Do all other cases here, and better errors.

        -- TODO: Re-introduce the type checks?
        liftF $ cc mempty
        cmd_mapping' (Lam (c,f)) r
--                
--                
--    -- cmd_mapping' (c,f)

-- | Run the command described by the list of bytestrings, feeding stdin
-- and reading stdout.
cmdRaw :: [LB.ByteString] -> Raw x -> Raw x
cmdRaw args inp = cmd' args (\sin -> P.runEffect $ inp >-> P.toHandle sin)

cmd'
    :: [LB.ByteString]
    -> (Handle -> SafeT IO x)
    -> Raw x
cmd' args' inp = bracket
    ( createProcess (proc (head args) (tail args))
        { std_in = CreatePipe
        , std_err = Inherit
        , std_out = CreatePipe
        }
    )
    cleanupProcess
    $ \(Just sin, Just sout, Nothing, _) -> do
        a <- do
            PG.lift $ liftBaseWith $ \run -> do
                async $ do
                    r <- run $ inp sin
                    hClose sin
                    pure r
        P.fromHandle sout
        PG.lift $ do
            r <- liftIO $ wait a
            restoreM r
    where
        args = map C8.unpack args'

cmdSysSiso :: [LB.ByteString] -> Object x -> Object x
cmdSysSiso args inp = validate (cmdRaw args inp)

cmdSysSimo :: [LB.ByteString] -> Object x -> Many x
cmdSysSimo args inp = toObjects (cmdRaw args inp)

cmdSysMiso :: [LB.ByteString] -> Many x -> Object x
cmdSysMiso args inp = validate $ cmd' args (\sin -> emitObjects sin inp)

cmdSysMimo :: [LB.ByteString] -> Many x -> Many x
cmdSysMimo args inp = toObjects (cmd' args (\sin -> emitObjects sin inp))

newtype Lam = Lam (Int, [ Either LB.ByteString Int ])

data Nat = Z | S Nat

slurpNatObjs :: Int -> [LB.ByteString] -> FreeT Object (SafeT IO) r -> FreeT Object (SafeT IO) (FreeT Object (SafeT IO) r, [LB.ByteString])
slurpNatObjs 0 s i = pure (i,s)
slurpNatObjs n s f = do
    c <- PG.lift $ runFreeT f
    case c of
        Pure x -> if null s then pure (pure x,s) else error "Expected more objects"
        Free x -> do
            (arg, input') <- PG.lift $ P.toLazyM' x
            slurpNatObjs (pred n) (s ++ [arg]) input'

cmd_lines :: Object x -> FreeT Object (SafeT IO) x
cmd_lines o = o ^. P.lines


data Command
    = CSS SISO
    | CSM SIMO
    | CMS MISO
    | CMM MIMO
    | CMR MIRO
    | CSR SIRO
    | CRM RIMO
    | CNM NIMO

cmd_nil :: Many ()
cmd_nil = pure ()

cmd_cons :: LB.ByteString -> Many x -> Many x
cmd_cons arg input = do
    liftF $ P.fromLazy arg
    input

cmd_list :: [LB.ByteString] -> Many ()
cmd_list = mapM_ (liftF . P.fromLazy)

futil_list :: [ByteString] -> FutilCmd (Raw x -> Many ())
futil_list args = FutilCmd $ \_ -> mapM_ (liftF . P.yield) args

newtype SIRO = SIRO (forall x. Object x -> Raw x)
newtype MIRO = MIRO (forall x. Many x -> Raw x)
newtype SISO = SISO (forall x. Producer ByteString (SafeT IO) x -> Producer ByteString (SafeT IO) x)
newtype SIMO = SIMO (forall x. Producer ByteString (SafeT IO) x -> FreeT Object (SafeT IO) x)
newtype MISO = MISO (forall x. FreeT Object (SafeT IO) x -> Producer ByteString (SafeT IO) x)
newtype MIMO = MIMO (forall x. FreeT Object (SafeT IO) x -> FreeT Object (SafeT IO) x)
newtype RIMO = RIMO (forall x. Raw x -> Many x)
-- TODO: Should NI close stdin?
newtype NIMO = NIMO (Many ())

data Format
    = FConst !ByteString
    | FSArg
    deriving (Show)

parseFormat :: ByteString -> [Format]
parseFormat f = case M.parse parser "" f of
    Left _ -> error "format error"
    Right r -> r

    where
        parser :: M.Parsec Void ByteString [Format]
        parser = M.many (pConst <|> pArg)
        
        pConst :: M.Parsec Void ByteString Format
        pConst = do
            ws <- M.some (M.noneOf ([37,92] :: [Word8]) <|> pEsc)
            pure (FConst (ByteString.pack ws))

        pEsc = do
            let
                escMap :: [(Word8, Word8)]
                escMap = map (\(a,b) -> (fromIntegral $ ord a, fromIntegral $ ord b))
                    [ ('n', '\n')
                    , ('t', '\t')
                    ]
            M.char 92
            c <- M.oneOf (map fst escMap)
            pure (fromJust $ lookup c escMap)

        pArg :: M.Parsec Void ByteString Format
        pArg = do
            M.char 37
            M.char 115
            pure FSArg
            

-- | Format @`Many`@ @`Object`@s using a format string.
-- For each format parameter, pull an object out of the
-- stream and place it in the corresponding location.
--
-- * @\\n@ - newline
-- * @%s@ - output one object
--
-- The format string is repeated as often as is required
-- to consume all the objects.
--
-- If we run out of objects half way through, we simple
-- stop early. No error is raised. ? Is this what we want?
--
-- NB: This is likely to change a lot.
futil_format :: ByteString -> FutilCmd (Many x -> Raw x)
futil_format fmt = FutilCmd $ go theFormat
    where
        theFormat :: [Format]
        theFormat = Main.parseFormat fmt

        go :: [Format] -> Many x -> Raw x
        go [] inp = go theFormat inp
        go ((FConst bs):fs) inp = do
            P.yield bs
            go fs inp
        go (FSArg:fs) inp = do
            c <- lift $ runFreeT inp
            case c of
                Pure x -> pure x
                Free x -> do
                    r <- x
                    go fs r


cmd_bind :: SIMO -> MIMO
cmd_bind (SIMO k) = MIMO go
    where
        go :: forall x. Many x -> Many x
        go inp = do
            c <- PG.lift $ runFreeT inp
            case c of
                Pure x -> pure x
                Free x -> do
                    r <- k x
                    go r

cmd_mapIO :: [LB.ByteString] -> Many x -> Many x
cmd_mapIO cmda input = do
    c <- PG.lift $ runFreeT input
    case c of
        Pure x -> pure x
        Free x -> do
            r <- liftF $ cmdSysSiso cmda x
            cmd_mapIO cmda r

cmd_mapIO' :: SISO -> FreeT Object (SafeT IO) x -> FreeT Object (SafeT IO) x
cmd_mapIO' ss@(SISO s) input = do
    c <- PG.lift $ runFreeT input
    case c of
        Pure x -> pure x
        Free x -> do
            r <- liftF $ s x -- fmap fromJust $ cmd cmda (Just x)
            cmd_mapIO' ss r


futil_take :: Int -> FutilCmd (Many x -> Many ())
futil_take i = FutilCmd (cmd_take i)

-- instance Futil ByteString where
--     cmd args "take" = cmd args futil_take
--     cmd args "rpn"  = cmd args futil_rpn
--     cmd args "unlines" = cmd args futil_unlines
--     cmd args "lines" = cmd args (FutilCmd cmd_lines)
--     cmd args "list" = cmd args futil_list
--     cmd args raw = FutilCmd $ cmdRaw (C8.fromStrict <$> (raw : args))

instance Futil a => Futil (Int -> a) where
    cmd (a:args) f = case SC8.readInt a of
        Just (i,"") -> cmd args (f i)
        _           -> error $ "Not an integer: " ++ show a
    cmd _ _ = error "Expected an argument"

instance Futil a => Futil ([ByteString] -> a) where
    cmd args f = cmd [] (f args)

instance Futil a => Futil (ByteString -> a) where
    cmd (arg:args) f = cmd args (f arg)

instance (FromRaw b, FromRaw a) => Futil (FutilCmd (a -> b)) where
    cmd [] (FutilCmd f) = FutilCmd $ (\x -> toRaw $ f (fromRaw x))
    cmd (ncmd:args) (FutilCmd f) =
        let
            FutilCmd input = cmd args ncmd
        in
            FutilCmd $ (\x -> input (toRaw $ f (fromRaw x)))

class FromRaw a where
    fromRaw :: Raw () -> a
    toRaw :: a -> Raw ()

instance x ~ () => FromRaw (Object x) where
    fromRaw = validate
    toRaw = id

instance x ~ () => FromRaw (Many x) where
    fromRaw = toObjects
    toRaw = output

futil_nil :: FutilCmd (Raw () -> Raw ())
futil_nil = FutilCmd $ \i -> mempty

futil_cons :: ByteString -> FutilCmd (Many x -> Many x)
futil_cons a = FutilCmd $ \inp -> do
    liftF $ P.yield a
    inp

-- TODO: For Many x, flush after each object.
runFutile :: ByteString -> [ByteString] -> IO ()
runFutile a args = runSafeT $ P.runEffect $ unFutil (cmd args a) P.stdin >-> P.stdout

futil_unlines :: FutilCmd (Many x -> Raw x)
futil_unlines = FutilCmd $ \x -> x ^. P.unlines

futil_lines :: FutilCmd (Raw x -> Many x)
futil_lines = FutilCmd $ \x -> x ^. P.lines

commandToRaw :: Main.Command -> Raw x -> Raw x
commandToRaw (CSS (SISO x)) = x
commandToRaw (CMM (MIMO x)) = output . x . toObjects
commandToRaw (CSM (SIMO x)) = output . x
commandToRaw (CMS (MISO x)) = x . toObjects
commandToRaw (CMR (MIRO x)) = x . toObjects
commandToRaw (CSR (SIRO x)) = x
commandToRaw (CRM (RIMO x)) = output . x
-- commandToRaw (CNM (NIMO x)) = _

runCmd :: Main.Command -> IO ()
runCmd (CSS (SISO x)) = void $ runSafeT $ P.runEffect $ x P.stdin >-> P.stdout
runCmd (CMM (MIMO x)) = void $ runSafeT $ emitObjects stdout $ x (P.stdin ^. objects)
runCmd (CSM (SIMO x)) = void $ runSafeT $ emitObjects stdout $ x (validate P.stdin)
runCmd (CMS (MISO x)) = void $ runSafeT $ P.runEffect $ x (P.stdin ^. objects) >-> P.stdout
runCmd (CMR (MIRO x)) = void $ runSafeT $ P.runEffect $ x (P.stdin ^. objects) >-> P.stdout
runCmd (CSR (SIRO x)) = void $ runSafeT $ P.runEffect $ x P.stdin >-> P.stdout
runCmd (CRM (RIMO x)) = void $ runSafeT $ emitObjects stdout $ x P.stdin 
runCmd (CNM (NIMO x)) = void $ runSafeT $ emitObjects stdout x


lamFromList :: [LB.ByteString] -> Either String Lam
lamFromList i =
    case Data.List.break (==":") i of
        (cmd, []) -> pure $ Lam (1, map Left cmd ++ [Right 0])
        (arg, ":" : []) -> Left "Need at least one command to execute after the ':'"
        (arg, ":" : cmds)
            | arg /= nub arg -> Left "Duplicate argument names."
            | otherwise      -> pure $ Lam (length arg, do
                c <- cmds
                pure $ maybe (Left c) Right $ elemIndex c arg
                )

cmd_take :: Int -> Many x -> Many ()
cmd_take 0 _ = pure ()
cmd_take n i = do
    x <- lift $ runFreeT i
    case x of
        Pure _ -> pure () -- perhaps we should error out. Use truncate as a version which doesn't?
        Free f -> do
            r <- liftF f
            cmd_take (pred n) r

futil_rpn :: FutilCmd (Many x -> Many x)
futil_rpn = FutilCmd cmd_rpn

cmd_rpn :: Many x -> Many x
cmd_rpn = go []
    where
        pushStack (a:b:cs) "+" = (a + b) : cs
        pushStack a b = case C8.readInt b of
            Just (i, "") -> i : a
            _            -> error $ "Failed to parse: " ++ show b ++ ", " ++ show a

        go :: [Int] -> Many x -> Many x
        go stack i = do
            x <- lift $ runFreeT i
            case x of
                Pure x -> do
                    mapM_ (liftF . P.fromLazy . C8.pack . show) stack
                    pure x
                Free f -> do
                    (r, a) <- lift $ P.toLazyM' f
                    go (pushStack stack r) a

realMain :: IO ()
realMain = do
    (a:as) <- System.Posix.Env.ByteString.getArgs
    runFutile a as

main :: IO ()
main = do
    n <- getProgName
    a <- Env.getArgs
    case n of
        "futil" -> realMain
        "<interactive>" -> realMain
        x       -> withArgs (x : a) realMain

discoverFutil
