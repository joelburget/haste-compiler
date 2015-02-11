module Main (main) where

import GHC
import HscMain
import Outputable (showPpr)
import DynFlags
import TidyPgm
import CorePrep
import CoreToStg
import StgSyn (StgBinding)
import HscTypes
import GhcMonad
import Module (packageIdString)
import System.Environment (getArgs)
import Control.Monad (when, forM_)
import Haste
import Haste.Args
import Haste.Opts
import Haste.Environment
import Haste.Module
import Haste.Version
import Data.JSTarget.AST
import System.IO
import System.Exit (exitFailure)
import Data.Version
import Data.List
import qualified Control.Shell as Sh
import Name (getOccString)
import qualified Data.ByteString.Lazy.Char8 as B8
import Haste.Compile
import System.Directory
import Digraph


main :: IO ()
main = do
    -- (path, fileHandle) <- mkstemps "streaming" ".hs"
    (path, fileHandle) <- openTempFile "." "streaming.hs"
    code <- B8.hGetContents stdin
    B8.hPut fileHandle code

    -- Parse static flags (TODO(joel) is this necessary)
    (ghcargs', _) <- parseStaticFlags $ map noLoc
        [ "-O2"
        , "-no-global-package-db"
        , "-no-user-package-db"
        , "-package-db " ++ pkgSysDir
        , "-package-db " ++ pkgUserDir
        , path
        ]

    runGhc (Just ghcLibDir) $ do
      -- Handle dynamic GHC flags. Make sure __HASTE__ is #defined.
      let hastever = "-D__HASTE__=" ++ show intVersion
          args = hastever : map unLoc ghcargs'
          justDie = const $ liftIO exitFailure
      dynflags <- getSessionDynFlags
      defaultCleanupHandler dynflags $ handleSourceError justDie $ do
        (dynflags', [file], _) <- parseDynamicFlags dynflags (map noLoc args)
        _ <- setSessionDynFlags dynflags' {ghcLink = NoLink,
                                          -- XXX changed from OneShot
                                           ghcMode = CompManager}

        -- Prepare and compile all needed targets.
        let file' = unLoc file
            printErrorAndDie e = printException e >> liftIO exitFailure
        deps <- handleSourceError printErrorAndDie $ do
            t <- guessTarget file' Nothing
            setTargets [t]
            _ <- load LoadAllTargets
            depanal [] False

        let deps' = flattenSCCs $ topSortModuleGraph True deps Nothing
            targetMod = last deps'
            otherMods = init deps'
            othermodPaths = commonJsUserDir

        mapM_ (compileDep othermodPaths dynflags') otherMods
        str <- compileMain othermodPaths dynflags' targetMod
        liftIO $ B8.putStrLn str

    fromRight "remove temp files" . Sh.shell $ do
        -- remove temp file .hs, .hi, .o
        -- TODO this doesn't always work?
        Sh.rm path
        Sh.rm (Sh.replaceExtension path ".hi")
        Sh.rm (Sh.replaceExtension path ".o")
