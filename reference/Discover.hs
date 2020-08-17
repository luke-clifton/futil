{-# LANGUAGE TemplateHaskell #-}
module Discover where

import Pipes
import qualified Pipes.ByteString as P
import Futil
import Data.Foldable
import Data.Char (isSpace)
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ord as Ord
import Language.Haskell.TH (Exp(..), Q, TExp, location, runIO, Dec(..), Match(..), Body(..), Pat(..), Lit(..), newName)
import Language.Haskell.TH.Syntax (Loc(..), mkName, unTypeQ, unsafeTExpCoerce, lift, Name)
import qualified Data.ByteString.Lazy as C8
import Data.ByteString (ByteString)

getCurrentFile :: Q FilePath
getCurrentFile = loc_filename <$> location

discoverFutilMap :: Q [(String, Name)]
discoverFutilMap = do
    file <- getCurrentFile
    lines <- lines <$> runIO (readFile file)
    let
      cmds = do
          line <- lines
          nopre <- toList $ List.stripPrefix "futil_" line
          let (cmd,rest) = span (not . isSpace) nopre
          guard (" ::" `List.isPrefixOf` rest)
          pure $ (cmd, mkName ("futil_" ++ cmd))
          
    pure cmds

discoverFutilCase :: Q Exp
discoverFutilCase = do
    m <- discoverFutilMap
    ms <- mapM makeMatch m
    rawcase <- rawCase
    futilcase <- futilCase
    pure $ LamCaseE (ms ++ [futilcase, rawcase])

    where
        rawCase :: Q Match
        rawCase = do
            n <- newName "cmd"
            a <- [| let 
                        cmdline = C8.fromStrict <$> ( $(pure $ VarE n) : args)
                        r = cmdRaw cmdline
                        cr = cmd' cmdline
                    in FutilExe
                        { exeRaw = r
                        -- TODO: Use exeRaw to feed exeO handle somehow. This
                        -- achieves flushing objects to subprocesses.
                        , exeO = \h i -> runSafeT $ runEffect $ r i >-> P.toHandle h
                        }

                 |]
            pure $ Match (VarP n) (NormalB a) []

        futilCase :: Q Match
        futilCase = do
            a <- [|cmd (tail args) (head args)|]
            pure $ Match (LitP (StringL "futil")) (NormalB a) []

        makeMatch :: (String, Name) -> Q Match
        makeMatch (s, n) = do
            a <- [|cmd args $(pure $ VarE n)|]
            pure $ Match (LitP (StringL s)) (NormalB a) []

--    cmd (ncmd:args) (FutilCmd f) =
--        let
--            cont = cmd args ncmd
--            r = \x -> toRaw (f (fromRaw x))
--        in
--            FutilExe
--                { exeO = \h i -> exeO cont h (r i)
--                , exeRaw = \i -> exeRaw cont (r i)
--                }

discoverFutil :: Q [Dec]
discoverFutil = [d|
 instance Futil ByteString where
    cmd args = $(discoverFutilCase)
 |]
