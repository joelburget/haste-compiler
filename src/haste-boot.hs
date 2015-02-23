{-# LANGUAGE CPP #-}
import Prelude hiding (read)
import Network.HTTP
import Network.Browser hiding (err)
import Network.URI
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Data.Maybe (fromJust)
import Codec.Compression.BZip
import Codec.Archive.Tar
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Haste.Environment
import Haste.Version
import Control.Shell
import Data.Char (isDigit)
import Control.Monad.IO.Class (liftIO)
import Haste.Args
import System.Console.GetOpt

#if __GLASGOW_HASKELL__ >= 708
baseDir = "base-ghc-7.8"
#else
baseDir = "base-ghc-7.6"
#endif

downloadFile :: String -> Shell BS.ByteString
downloadFile f = do
  (_, rsp) <- liftIO $ Network.Browser.browse $ do
    setAllowRedirects True
    request $ Request {
        rqURI = fromJust $ parseURI f,
        rqMethod = GET,
        rqHeaders = [],
        rqBody = BS.empty
      }
  case rspCode rsp of
    (2, _, _) -> return $ rspBody rsp
    _         -> fail $ "Failed to download " ++ f ++ ": " ++ rspReason rsp

data Cfg = Cfg {
    getLibs               :: Bool,
    getClosure            :: Bool,
    useLocalLibs          :: Bool,
    tracePrimops          :: Bool,
    forceBoot             :: Bool,
    populateSetupExeCache :: Bool
  }

defCfg :: Cfg
defCfg = Cfg {
    getLibs = True,
    getClosure = True,
    useLocalLibs = False,
    tracePrimops = False,
    forceBoot = False,
    populateSetupExeCache = True
  }

devBoot :: Cfg -> Cfg
devBoot cfg = cfg {
    useLocalLibs          = True,
    forceBoot             = True,
    getClosure            = False,
    populateSetupExeCache = False
  }

specs :: [OptDescr (Cfg -> Cfg)]
specs = [
    Option "" ["dev"]
           (NoArg devBoot) $
           "Boot Haste for development. Implies --force " ++
           "--local --no-closure --no-populate-setup-exe-cache",
    Option "" ["force"]
           (NoArg $ \cfg -> cfg {forceBoot = True}) $
           "Re-boot Haste even if already properly booted.",
    Option "" ["local"]
           (NoArg $ \cfg -> cfg {useLocalLibs = True}) $
           "Use libraries from source repository rather than " ++
           "downloading a matching set from the Internet. " ++
           "This is nearly always necessary when installing " ++
           "Haste from Git rather than from Hackage. " ++
           "When using --local, your current working directory " ++
           "must be the root of the Haste source tree.",
    Option "" ["no-closure"]
           (NoArg $ \cfg -> cfg {getClosure = False}) $
           "Don't download Closure compiler. You won't be able " ++
           "to use --opt-minify, unless you manually " ++
           "give hastec the path to compiler.jar.",
    Option "" ["no-libs"]
           (NoArg $ \cfg -> cfg {getLibs = False}) $
           "Don't install any libraries. This is probably not " ++
           "what you want.",
    Option "" ["no-populate-setup-exe-cache"]
           (NoArg $ \cfg -> cfg {populateSetupExeCache = False}) $
           "Don't populate Cabal's setup-exe-cache. Speeds up boot, " ++
           "but will fail spectacularly unless your setup-exe-cache " ++
           "is already populated.",
    Option "" ["trace-primops"]
           (NoArg $ \cfg -> cfg {tracePrimops = True}) $
           "Build standard libs for tracing of primitive " ++
           "operations. Only use if you're debugging the code " ++
           "generator."
  ]

hdr :: String
hdr = "Fetch, build and install all libraries necessary to use Haste.\n"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs specs hdr args of
    Right (mkConfig, _) -> do
      let cfg = mkConfig defCfg
      when (needsReboot || forceBoot cfg) $ do
        res <- shell $ if useLocalLibs cfg
                         then bootHaste cfg "."
                         else withTempDirectory "haste" $ bootHaste cfg
        case res of
          Right _  -> return ()
          Left err -> putStrLn err >> exitFailure
    Left halp -> do
      putStrLn halp

bootHaste :: Cfg -> FilePath -> Shell ()
bootHaste cfg tmpdir = inDirectory tmpdir $ do
  removeBootFile <- isFile bootFile
  when removeBootFile $ rm bootFile
  when (getLibs cfg) $ do
    when (populateSetupExeCache cfg) $ do
      void $ run "cabal" ["update"] ""
      void $ run "cabal" ["install", "-j", "populate-setup-exe-cache"] ""
      void $ run "ghc-pkg" ["unregister", "populate-setup-exe-cache"] ""
    when (not $ useLocalLibs cfg) $ do
      fetchLibs tmpdir
    mapM_ clearDir [hasteInstUserDir, jsmodUserDir, commonJsUserDir, pkgUserDir,
                    hasteInstSysDir, jsmodSysDir, commonJsSysDir, pkgSysDir]
    buildLibs cfg
    when (portableHaste) $ do
      mapM_ relocate ["array", "bytestring", "containers", "data-default",
                      "data-default-class", "data-default-instances-base",
                      "data-default-instances-containers",
                      "data-default-instances-dlist",
                      "data-default-instances-old-locale",
                      "deepseq", "dlist", "haste-lib", "integer-gmp",
                      "monads-tf", "old-locale", "transformers"]
  when (getClosure cfg) $ do
    installClosure
  file bootFile (showBootVersion bootVersion)

clearDir :: FilePath -> Shell ()
clearDir dir = do
  exists <- isDirectory dir
  when exists $ rmdir dir


-- | Fetch the Haste base libs.
fetchLibs :: FilePath -> Shell ()
fetchLibs tmpdir = do
    echo "Downloading base libs from GitHub"
    file <- downloadFile $ mkUrl hasteVersion
    liftIO . unpack tmpdir . read . decompress $ file
  where
    mkUrl v =
      "http://valderman.github.io/haste-libs/haste-libs-" ++ showVersion v ++ ".tar.bz2"

-- | Fetch and install the Closure compiler.
installClosure :: Shell ()
installClosure = do
    echo "Downloading Google Closure compiler..."
    downloadClosure `orElse` do
      echo "Couldn't install Closure compiler; continuing without."
  where
    downloadClosure = do
      downloadFile closureURI >>= (liftIO . BS.writeFile closureCompiler)
    closureURI =
      "http://valderman.github.io/haste-libs/compiler.jar"

-- | Build haste's base libs.
buildLibs :: Cfg -> Shell ()
buildLibs cfg = do
    -- Set up dirs and copy includes
    mkdir True $ pkgSysLibDir
    cpDir "include" hasteSysDir
    run_ hastePkgBinary ["update", "--global", "libraries" </> "rts.pkg"] ""

    inDirectory "libraries" $ do
      -- Install ghc-prim
      inDirectory "ghc-prim" $ do
        hasteInst ["configure", "--solver", "topdown"]
        hasteInst $ ["build", "--install-jsmods"] ++ ghcOpts
        run_ hasteInstHisBinary ["ghc-prim-0.3.0.0", "dist" </> "build"] ""
        run_ hastePkgBinary ["update", "--global", "packageconfig"] ""

      -- Install integer-gmp; double install shouldn't be needed anymore.
      run_ hasteCopyPkgBinary ["Cabal"] ""
      inDirectory "integer-gmp" $ do
        hasteInst ("install" : "--solver" : "topdown" : ghcOpts)

      -- Install base
      inDirectory baseDir $ do
        basever <- file "base.cabal" >>= return
          . dropWhile (not . isDigit)
          . head
          . filter (not . null)
          . filter (and . zipWith (==) "version")
          . lines
        hasteInst ["configure", "--solver", "topdown"]
        hasteInst $ ["build", "--install-jsmods"] ++ ghcOpts
        let base = "base-" ++ basever
            pkgdb = "--package-db=dist" </> "package.conf.inplace"
        run_ hasteInstHisBinary [base, "dist" </> "build"] ""
        run_ hasteCopyPkgBinary [base, pkgdb] ""
        forEachFile "include" $ \f -> cp f (hasteSysDir </> "include")

      -- Install array and haste-lib
      forM_ ["array", "haste-lib"] $ \pkg -> do
        inDirectory pkg $ hasteInst ("install" : ghcOpts)

      -- Export monads-tf; it seems to be hidden by default
      run_ hastePkgBinary ["expose", "monads-tf"] ""
  where
    ghcOpts = concat [
        if tracePrimops cfg then ["--ghc-option=-debug"] else [],
        ["--ghc-option=-DHASTE_HOST_WORD_SIZE_IN_BITS=" ++ show hostWordSize]
      ]
    hasteInst args =
      run_ hasteInstBinary ("--install-global" : "--unbooted" : args) ""

relocate :: String -> Shell ()
relocate pkg = run_ hastePkgBinary ["relocate", pkg] ""
