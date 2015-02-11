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
import Haste.Compile
import Haste.Opts
import Haste.Environment
import Haste.Version
import System.IO
import System.Exit (exitFailure)
import Data.Version
import Data.List
import qualified Control.Shell as Sh
import Name (getOccString)

logStr :: Config -> String -> IO ()
logStr cfg = when (verbose cfg) . hPutStrLn stderr

rebootMsg :: String
rebootMsg = "Haste needs to be rebooted; please run haste-boot"

printInfo :: IO ()
printInfo = do
  ghc <- runGhc (Just ghcLibDir) getSessionDynFlags
  putStrLn $ formatInfo $ compilerInfo ghc
  where
    formatInfo = ('[' :) . tail . unlines . (++ ["]"]) . map ((',' :) . show)

-- | Check for arguments concerning version info and the like, and act on them.
--   Return True if the compiler should run afterwards.
preArgs :: [String] -> IO Bool
preArgs args
  | "--numeric-version" `elem` args =
    putStrLn (showVersion ghcVersion) >> return False
  | "--info" `elem` args =
    printInfo >> return False
  | "--print-libdir" `elem` args =
    putStrLn ghcLibDir >> return False
  | "--version" `elem` args =
    putStrLn (showVersion hasteVersion) >> return False
  | "--supported-extensions" `elem` args =
    (putStrLn $ unlines $ supportedLanguagesAndExtensions) >> return False
  | "--supported-languages" `elem` args =
    (putStrLn $ unlines $ supportedLanguagesAndExtensions) >> return False
  | otherwise =
    return True

main :: IO ()
main = do
    initUserPkgDB
    args <- fmap (++ packageDBArgs) getArgs
    runCompiler <- preArgs args
    when runCompiler $ do
      if allSupported args
        then hasteMain args
        else callVanillaGHC args
  where
    packageDBArgs = ["-no-global-package-db",
                     "-no-user-package-db",
                     "-package-db " ++ pkgSysDir,
                     "-package-db " ++ pkgUserDir ]

-- | Call vanilla GHC; used for boot files and the like.
callVanillaGHC :: [String] -> IO ()
callVanillaGHC args = do
  _ <- Sh.shell $ Sh.run_ ghcBinary (filter noHasteArgs args) ""
  return ()
  where
    noHasteArgs x =
      x /= "--libinstall" &&
      x /= "--unbooted"

initUserPkgDB :: IO ()
initUserPkgDB = do
  _ <- Sh.shell $ do
    pkgDirExists <- Sh.isDirectory pkgUserDir
    when (not pkgDirExists) $ do
      Sh.mkdir True pkgUserLibDir
      Sh.runInteractive ghcPkgBinary ["init", pkgUserDir]
  return ()

-- | Run the compiler if everything's satisfactorily booted, otherwise whine
--   and exit.
hasteMain :: [String] -> IO ()
hasteMain args
  | not needsReboot =
    compiler False ("-O2" : args)
  | otherwise = do
    if "--unbooted" `elem` args
      then compiler True (filter (/= "--unbooted") ("-O2" : args))
      else fail rebootMsg

-- | Determine whether all given args are handled by Haste, or if we need to
--   ship them off to vanilla GHC instead.
allSupported :: [String] -> Bool
allSupported args =
  and args'
  where
    args' = [not $ any (`isSuffixOf` a) someoneElsesProblems | a <- args]
    someoneElsesProblems = [".c", ".cmm"]

-- | The main compiler driver.
compiler :: Bool -> [String] -> IO ()
compiler unbooted cmdargs = do
  let argRes = parseArgs (hasteOpts unbooted) helpHeader cmdargs
      usedGhcMode = if "-c" `elem` cmdargs then OneShot else CompManager

  case argRes of
    -- We got --help as an argument - display help and exit.
    Left help -> putStr help

    -- We got a config and a set of arguments for GHC; let's compile!
    Right (mkConfig, ghcargs) -> do
      let config = mkConfig def

      -- Parse static flags, but ignore profiling.
      (ghcargs', _) <- parseStaticFlags [noLoc a | a <- ghcargs, a /= "-prof"]

      runGhc (Just ghcLibDir) $ do
        -- Handle dynamic GHC flags. Make sure __HASTE__ is #defined.
        let hastever = "-D__HASTE__=" ++ show intVersion
            args = hastever : map unLoc ghcargs'
            justDie = const $ liftIO exitFailure
        dynflags <- getSessionDynFlags
        defaultCleanupHandler dynflags $ handleSourceError justDie $ do
          (dynflags', files, _) <- parseDynamicFlags dynflags (map noLoc args)
          _ <- setSessionDynFlags dynflags' {ghcLink = NoLink,
                                             ghcMode = usedGhcMode}

          -- Prepare and compile all needed targets.
          let files' = map unLoc files
              printErrorAndDie e = printException e >> liftIO exitFailure
          deps <- handleSourceError printErrorAndDie $ do
            ts <- mapM (flip guessTarget Nothing) files'
            setTargets ts
            _ <- load LoadAllTargets
            depanal [] False
          let cfg = fillLinkerConfig dynflags' config
          mapM_ (compileDep (commonJsLibPath cfg) dynflags') deps

          -- Link everything together into a .js file.
          when (performLink cfg) $ liftIO $ do
            forM_ files' $ \file -> do
              let outfile = outFile cfg cfg file
              logStr cfg $ "Linking program " ++ outfile
              let pkgid = showPpr dynflags $ thisPackage dynflags'
              link cfg pkgid file
              -- case useGoogleClosure cfg of
              --   Just clopath -> closurize cfg clopath outfile
              --   _            -> return ()
              -- when (outputHTML cfg) $ do
              --   res <- Sh.shell $ Sh.withCustomTempFile "." $ \tmp h -> do
              --     prog <- Sh.file outfile
              --     Sh.hPutStrLn h (htmlSkeleton outfile prog)
              --     Sh.liftIO $ hClose h
              --     Sh.mv tmp outfile
              --   case res of
              --     Right () -> return ()
              --     Left err -> error $ "Couldn't output HTML file: " ++ err


-- | Fill in linkage info, such as whether to link at all and what the program
--   entry point is.
fillLinkerConfig :: DynFlags -> Config -> Config
fillLinkerConfig df cfg =
    cfg {
        mainMod = mainmod,
        performLink = maybe False (const $ performLink cfg) mainmod
      }
  where
    mainmod =
      Just (packageIdString $ modulePackageId (mainModIs df),
            moduleNameString $ moduleName (mainModIs df))
