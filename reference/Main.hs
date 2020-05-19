{-# LANGUAGE OverloadedStrings #-}
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

-- Naming convention.
--
-- withXXX -- the std input is expected to be encoded as an XXX, this will be
--            decoded and then passed to continuation.
-- 
-- xxx     -- If arguments are provided, perform xxx on the output of the
--            command, otherwise perform xxx on stdinput.
--
-- unXxx   -- Do the opposite of xxx

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


-- | A @`Raw`@ is a raw stream of bytes. Any byte can appear in a raw stream.
type Raw  = Raw' (SafeT IO)
type Raw' = Producer ByteString

-- | An @`Object`@ is a stream of bytes that does not include the @\0@ byte.
type Object  = Object' (SafeT IO)
type Object' = P.Producer ByteString

-- | A @`Many`@ is a stream of @`Objects`@s delimited by @\0@ bytes.
type Many = Many' (SafeT IO)
type Many' m = FreeT (Object' m) m


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
validate :: Functor m => Raw' m x -> Object' m x
validate p = P.for p $ \b -> do
    when (ByteString.elem 0 b) $ fail "Invalid object. Contains '\\0' byte."
    P.yield b

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

-- | Similar to @`cmd_mapping`@, except that the @`Lam`@ type is used to
-- represent a "function" that can be constructed at run time.
cmd_mapping' :: Lam -> FreeT Object (SafeT IO) r -> FreeT Object (SafeT IO) r
cmd_mapping' (c,f) input = do
    -- Check if there is another object, if so, we apply, otherwise we stop.
    -- `apply` will pull of as many objects as there are arguments.
    (r,bs) <- slurpNatObjs c [] input
    if null bs
    then r
    else do
        let
            ff = do
                c <- f
                case c of
                    Left b -> pure b
                    Right ix -> pure $ bs !! ix
        liftF $ validate $ cmd ff Nothing
        cmd_mapping' (c,f) r
    -- cmd_mapping' (c,f)

cmd :: [LB.ByteString] -> Maybe (Producer ByteString (SafeT IO) x) -> Producer ByteString (SafeT IO) (Maybe x)
cmd args' minp = bracket
    ( createProcess (proc (head args) (tail args))
        { std_in = maybe NoStream (const CreatePipe) minp
        , std_err = Inherit
        , std_out = CreatePipe
        }
    )
    cleanupProcess
    $ \(msin, Just sout, Nothing, _) -> do
        a <- fromMaybe (pure Nothing) $ do
            sin <- msin
            inp <- minp
            pure $ PG.lift $ liftBaseWith $ \run -> do
                a <- async $ do
                    r <- run $ P.runEffect $ inp >-> P.toHandle sin
                    hClose sin
                    pure r
                pure (Just a)
        P.fromHandle sout
        PG.lift $ forM a $ \ac -> do
            r <- liftIO $ wait ac
            restoreM r
    where
        args = map C8.unpack args'


type Lam = (Int, [ Either LB.ByteString Int ])

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
cmd_lines o = (validate o) ^. P.lines

cmd_lines' :: [LB.ByteString] -> Object () -> IO () -- Object ()
cmd_lines' c o = runSafeT $ P.runEffect $ fmap (const ()) (cmd c (Just (output $ (validate o) ^. P.lines))) >-> P.stdout

cmd_mapIO :: [LB.ByteString] -> FreeT Object (SafeT IO) x -> FreeT Object (SafeT IO) x
cmd_mapIO cmda input = do
    c <- PG.lift $ runFreeT input
    case c of
        Pure x -> pure x
        Free x -> do
            r <- liftF $ fmap fromJust $ cmd cmda (Just x)
            cmd_mapIO cmda r


arg :: LB.ByteString -> ArgParse (IO ()) -> ArgParse (IO ())
arg a io = do
    a' <- shift
    unless (a == a') $ fail $ "Expected " ++ show a ++ " but got " ++ show a'
    io

pLambda :: ArgParse Lam
pLambda = do
    as <- get
    ExceptT $ pure $ lamFromList as

shift :: ArgParse LB.ByteString
shift = do
    as <- get
    case as of
        a:rest -> put rest >> pure a
        []     -> fail "Expecting more arguments."

select :: [ (LB.ByteString, ArgParse a) ] -> ArgParse a
select s = do
    a <- shift
    case List.lookup a s of
        Nothing -> throwError $ "Expected one of " ++ show (map fst s)
        Just x -> x

data Command
    = CSS SISO
    | CSM SIMO
    | CMS MISO
    | CMM MIMO

newtype SISO = SISO (forall x. Producer ByteString (SafeT IO) x -> Producer ByteString (SafeT IO) x)
newtype SIMO = SIMO (forall x. Producer ByteString (SafeT IO) x -> FreeT Object (SafeT IO) x)
newtype MISO = MISO (forall x. FreeT Object (SafeT IO) x -> Producer ByteString (SafeT IO) x)
newtype MIMO = MIMO (forall x. FreeT Object (SafeT IO) x -> FreeT Object (SafeT IO) x)

class Parse a t where
    p :: a -> ArgParse t

instance Parse x t => Parse (LB.ByteString -> x) t where
    p f = do
        as <- get
        case as of
            [] -> fail "Expected an argument"
            (a:rest) -> put rest >> p (f a)

instance Parse x t => Parse ([LB.ByteString] -> x) t where
    p f = do
        as <- get
        put []
        p (f as)

instance Parse a t => Parse (SISO -> a) t where
    p f = do
        save <- get
        s <- pSiso
        put []
        p (f s)

instance Parse MIMO MIMO where
    p f = pure f

cmd_mapIO' :: SISO -> FreeT Object (SafeT IO) x -> FreeT Object (SafeT IO) x
cmd_mapIO' ss@(SISO s) input = do
    c <- PG.lift $ runFreeT input
    case c of
        Pure x -> pure x
        Free x -> do
            r <- liftF $ s x -- fmap fromJust $ cmd cmda (Just x)
            cmd_mapIO' ss r

mimo :: [(LB.ByteString, ArgParse MIMO)]
mimo =
    [ ("map",   p $ \s -> MIMO $ cmd_mapIO' s )
    , ("mapping", do
        l <- pLambda -- TODO: Need to be able to fuse lambdas.
        p $ MIMO $ cmd_mapping' l
      )
    ]

siso :: [(LB.ByteString, ArgParse SISO)]
siso =
    [ ("identity", pure $ SISO id)
    ]

simo :: [(LB.ByteString, ArgParse Main.Command)]
simo =
    [ ("lines", do
        save <- get
        case save of
            [] -> pure $ CSM $ SIMO cmd_lines
            _  -> do
                c <- pRawCmd
                case c of
                    -- Left e -> pure $ CSM $ SIMO cmd_lines)
                    Left (CMS (MISO k)) -> pure $ CSS $ SISO (\x -> k (cmd_lines x))
                    Left (CMM (MIMO k)) -> pure $ CSM $ SIMO (\x -> k (cmd_lines x))
                    _ -> undefined
        )
    , ("words", do
            save <- get
            case save of
                [] -> pure $ CSM $ SIMO P.words
                _ -> do
                    c <- pRawCmd
                    case c of
                        Left (CMS (MISO k)) -> pure $ CSS $ SISO (\x -> k (P.words x))
                        Left _ -> error "nope"
                        Right x -> pure $ CSS $ SISO $ (\s -> fmap fromJust $ cmd x (Just (output (P.words s))))
      )
    ]

miso :: [(LB.ByteString, ArgParse MISO)]
miso =
    [ ("length", pure $ MISO $ \f -> do
        (i, x) <- lift $ cmd_length f
        P.yield (SC8.pack (show i))
        pure x
        )
    , ("unlines", pure $ MISO $ \x -> do
        -- unlines always line buffers.
        liftIO (hSetBuffering stdout LineBuffering)
        x ^. P.unlines)
    ]

pRawCmd :: ArgParse (Either Main.Command [LB.ByteString])
pRawCmd = do
    as <- get
    case as of
        [] -> throwError $ "Expected a command"
        (a:_) -> (Left <$> pCmd) <|> (put [] >> pure (Right as))

pSiso :: ArgParse SISO
pSiso = do
    save <- get
    ec <- pRawCmd
    case ec of
        Right c -> do
            when (head c == "find" && "-print0" `elem` c) $
                throwError $ "Using `find -print0` when we are expecting a command that does not produce '\\0' bytes."
            when (head c == "rm" && "--" `notElem` c) $ tell ["You should use `rm [options] -- [files]` to prevent bad things from happening"]
            pure $ SISO $ \x -> validate $ fmap fromJust $ cmd c (Just x)
        Left (CSS s) -> pure s
        Left _       -> throwError $ "The command `" ++ show save ++ "` is of the wrong type. Expected SISO"

pCmd :: ArgParse Main.Command
pCmd = select
    $  mk CSS siso
    ++ mk CMM mimo
    ++ simo
    ++ mk CMS miso
    where
        mk x = map (fmap $ fmap x)

runCmd :: Main.Command -> IO ()
runCmd (CSS (SISO x)) = void $ runSafeT $ P.runEffect $ x P.stdin >-> P.stdout
runCmd (CMM (MIMO x)) = void $ runSafeT $ emitObjects stdout $ x (P.stdin ^. objects)
runCmd (CSM (SIMO x)) = void $ runSafeT $ emitObjects stdout $ x P.stdin
runCmd (CMS (MISO x)) = void $ runSafeT $ P.runEffect $ x (P.stdin ^. objects) >-> P.stdout

type PareCtx x = (Int ~ Int)

runPCmd :: PareCtx x => [LB.ByteString] -> (Either String Main.Command, [String])
runPCmd = evalState (runWriterT $ runExceptT pCmd)

execCmd :: [LB.ByteString] -> IO ()
execCmd a = case runPCmd @() a of
    (Left e, _) -> do
        hPutStrLn stderr $ "error: " ++ e
        exitWith (ExitFailure 120)
    (Right e, warns) -> do
        traverse (\s -> hPutStrLn stderr $ "warning: " ++ s) warns
        runCmd e


lamFromList :: [LB.ByteString] -> Either String Lam
lamFromList i =
    case Data.List.break (==":") i of
        (cmd, []) -> pure (1, map Left cmd ++ [Right 0])
        (arg, ":" : []) -> Left "Need at least one command to execute after the ':'"
        (arg, ":" : cmds)
            | arg /= nub arg -> Left "Duplicate argument names."
            | otherwise      -> pure (length arg, do
                c <- cmds
                pure $ maybe (Left c) Right $ elemIndex c arg
                )


realMain :: IO ()
realMain = do
    -- fmap LB.fromStrict <$> System.Posix.Env.ByteString.getArgs >>= runOptions
    fmap LB.fromStrict <$> System.Posix.Env.ByteString.getArgs >>= execCmd

main :: IO ()
main = do
    n <- getProgName
    a <- Env.getArgs
    case n of
        "futil" -> realMain
        "<interactive>" -> realMain
        x       -> withArgs (x : a) realMain

    -- args <- fmap Lazy.fromStrict <$> getArgs

    -- case n of
    --     "mapping" -> mapping args
    --     "list" -> list args
    --     "zipping" -> zipping args
    --     "unlines" -> Main.unlines args
    --     -- TODO: The below us an example of performing an optimisation based
    --     -- on the arguments. Leaving it uncommented for now because I'd like
    --     -- to do extra checks (specifically, confirm that it is the same binary
    --     -- so that people can override it if they need to).
    --     --"unlines" -> case args of
    --     --    "lines":rest -> withFrozenCallStack $ exe rest
    --     --    _            -> Main.unlines args
    --     "lines" -> Main.lines args


list :: [Lazy.ByteString] -> IO ()
list args = do
    forM_ args $ \arg -> do
        Lazy.putStr arg
        Lazy.putStr "\0"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs =
    let
        (y, ys) = List.splitAt n xs
    in
        y : chunksOf n ys


mapping :: [Lazy.ByteString] -> IO ()
mapping prog = do
    let
        (vars, ":" : prog') = List.span (/= ":") prog
        nvars = length vars

    input <- chunksOf nvars . endBy0 <$> Lazy.getContents

    forM_ input $ \item -> do
        when (length item /= nvars) $ error "Mismatched number of arguments"
        let
            subs = zip vars item
            prog'' = map (\x -> fromMaybe x (List.lookup x subs)) prog'
        hFlush stdout
        withNullInput $ \n -> runProc' n stdout stderr (withFrozenCallStack $ exe prog'')
        Lazy.putStr "\0"

zipping :: [Lazy.ByteString] -> IO ()
zipping prog = do
    let
        (vars, ":" : prog') = List.span (/= ":") prog
        nvars = length vars

    input <- chunksOf nvars . endBy0 <$> Lazy.getContents

    forM_ input $ \item -> do
        let
            subs = zip vars item
            prog'' = map (\x -> fromMaybe x (List.lookup x subs)) prog'
        when (length item /= nvars) $ error "Mismatched number of arguments"
        forM_ item $ \i -> do
            Lazy.putStr i
            Lazy.putStr "\0"
        hFlush stdout
        withNullInput $ \n -> runProc' n stdout stderr (withFrozenCallStack $ exe prog'')
        Lazy.putStr "\0"


-- | selectSource commands either read from stdin if no arguments are provided
-- or execute the arguments and operate on their stdout.
--
-- The following are identical.
--
-- @
--  cat README | lines
--
--  lines cat README
-- @
--
selectSource :: Proc () -> [Lazy.ByteString] -> IO ()
selectSource cmd [] = runProc cmd
selectSource cmd args = withFrozenCallStack (exe args) |> cmd

unlines :: [Lazy.ByteString] -> IO ()
unlines = selectSource $ readInputEndBy0 (mapM_ Char8.putStrLn)

lines :: [Lazy.ByteString] -> IO ()
lines = selectSource $ readInputLines (mapM_ (\i -> Lazy.putStr i >> Lazy.putStr "\0"))


nest :: [Lazy.ByteString] -> IO ()
nest = selectSource $ pureProc (Lazy.concatMap encodingFn)

    where
        encodingFn :: Word8 -> Lazy.ByteString
        encodingFn 0 = "\1\1"
        encodingFn 1 = "\1\2"
        encodingFn c = Lazy.singleton c

--unNest :: [Lazy.ByteString] -> IO ()
--unNest = selectSource $ pureProc (fromList . go . Lazy.toList)
--
--    where
--        go (1:1:rest) = 0 : go rest
--        go (1:2:rest) = 1 : go rest
--        go (x:rest)   = x : go rest
