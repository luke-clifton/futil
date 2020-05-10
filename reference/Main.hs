{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = do
    n <- getProgName

    args <- fmap Lazy.fromStrict <$> getArgs

    case n of
        "mapping" -> mapping args
        "list" -> list args
        "zipping" -> zipping args
        "unlines" -> Main.unlines args
        -- TODO: The below us an example of performing an optimisation based
        -- on the arguments. Leaving it uncommented for now because I'd like
        -- to do extra checks (specifically, confirm that it is the same binary
        -- so that people can override it if they need to).
        --"unlines" -> case args of
        --    "lines":rest -> withFrozenCallStack $ exe rest
        --    _            -> Main.unlines args
        "lines" -> Main.lines args


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
