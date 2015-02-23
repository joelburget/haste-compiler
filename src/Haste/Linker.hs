{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Haste.Linker (link) where
import Haste.Config
import Haste.Module
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Control.Applicative
import Data.JSTarget
import qualified Data.ByteString.Lazy as B
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid
import System.IO (hPutStrLn, stderr)

-- | The program entry point.
--   This will need to change when we start supporting building "binaries"
--   using cabal, since we'll have all sorts of funny package names then.
mainSym :: Name
mainSym = name "main" (Just ("main", ":Main"))

-- | Link a program using the given config and input file name.
link :: Config -> String -> FilePath -> IO ()
link cfg pkgid target = do
  let mainmod = case mainMod cfg of
                 Just mm -> mm
                 _       -> error "Haste.Linker.link called without main sym!"
  ds <- getAllDefs cfg (targetLibPath cfg : libPaths cfg) mainmod pkgid mainSym
  let myDefs = if wholeProgramOpts cfg then topLevelInline ds else ds
      (progText, myMain') = prettyProg (ppOpts cfg) mainSym myDefs
      callMain = fromString "B(A(" <> myMain' <> fromString ", [0]));"
      launchApp = appStart cfg (fromString "hasteMain")

  rtslibs <- mapM readFile $ rtsLibs cfg
  extlibs <- mapM readFile $ jsExternals cfg
  B.writeFile (outFile cfg cfg target)
    $ toLazyByteString
    $ assembleProg (wrapProg cfg) extlibs rtslibs progText callMain launchApp
  where
    assembleProg True extlibs rtslibs progText callMain launchApp =
      (if useStrict cfg then fromString "\"use strict\";\n" else mempty)
      <> fromString (unlines extlibs)
      <> fromString "var hasteMain = function() {"
      <> fromString (unlines rtslibs)
      <> progText
      <> callMain
      <> fromString "};\n"
      <> launchApp
    assembleProg _ extlibs rtslibs progText callMain launchApp =
      (if useStrict cfg then fromString "\"use strict\";\n" else mempty)
      <> fromString (unlines extlibs)
      <> fromString (unlines rtslibs)
      <> progText
      <> fromString "\nvar hasteMain = function() {" <> callMain
                                                     <> fromString "};"
      <> launchApp

-- | Produce an info message if verbose reporting is enabled.
info' :: Config -> String -> IO ()
info' cfg = when (verbose cfg) . hPutStrLn stderr

-- | Generate a sequence of all assignments needed to run Main.main.
getAllDefs :: Config
           -> [FilePath]
           -> (String, String)
           -> String
           -> Name
           -> IO (AST Stm)
getAllDefs cfg libpaths mainmod pkgid mainsym =
  runDep cfg mainmod $ addDef libpaths pkgid mainsym

data DepState = DepState {
    mainmod     :: !(String, String),
    defs        :: !(AST Stm -> AST Stm),
    alreadySeen :: !(S.Set Name),
    modules     :: !(M.Map String Module),
    infoLogger  :: String -> IO ()
  }

type DepM a = EitherT Name (StateT DepState IO) a

initState :: Config -> (String, String) -> DepState
initState cfg m = DepState {
    mainmod     = m,
    defs        = id,
    alreadySeen = S.empty,
    modules     = M.empty,
    infoLogger  = info' cfg
  }

-- | Log a message to stdout if verbose reporting is on.
info :: String -> DepM ()
info s = do
  st <- get
  liftIO $ infoLogger st s

-- | Run a dependency resolution computation.
runDep :: Config -> (String, String) -> DepM a -> IO (AST Stm)
runDep cfg mainmod m = do
    res <- runStateT (runEitherT m) (initState cfg mainmod)
    case res of
      (Right _, st) ->
        return $ defs st nullRet
      (Left (Name f (Just (p, m))), _) -> do
        error $ msg m f
  where
    msg "Main" "main" =
      "Unable to locate a main function.\n" ++
      "If your main function is not `Main.main' you must specify it using " ++
      "`-main-is',\n" ++
      "for instance, `-main-is MyModule.myMain'.\n" ++
      "If your progam intentionally has no main function," ++
      " please use `--dont-link' to avoid this error."
    msg m f =
      "Unable to locate function `" ++ f ++ "' in module `" ++ m ++ "'!"

-- | Return the module the given variable resides in.
getModuleOf :: [FilePath] -> Name -> DepM Module
getModuleOf libpaths v@(Name n _) =
  case moduleOf v of
    Just "GHC.Prim" -> return foreignModule
    Just ""         -> return foreignModule
    Nothing         -> return foreignModule
    Just ":Main"    -> do
      (p, m) <- mainmod `fmap` get
      getModuleOf libpaths (Name n (Just (p, m)))
    Just m          -> do
      mm <- getModule libpaths (maybe "main" id $ pkgOf v) m
      case mm of
        Just m' -> return m'
        _       -> left v

-- | Return the module at the given path, loading it into cache if it's not
--   already there.
getModule :: [FilePath] -> String -> String -> DepM (Maybe Module)
getModule libpaths pkgid modname = do
    st <- get
    case M.lookup modname (modules st) of
      Just m -> do
        return $ Just m
      _ -> do
        info $ "Linking " ++ modname
        go libpaths
  where
    go (libpath:lps) = do
      mm <- liftIO $ readModule libpath pkgid modname
      case mm of
        Just m -> do
          st <- get
          put st {modules = M.insert modname m (modules st)}
          return (Just m)
        _ -> do
          go lps
    go [] = do
      return Nothing

-- | Add a new definition and its dependencies. If the given identifier has
--   already been added, it's just ignored.
addDef :: [FilePath] -> String -> Name -> DepM ()
addDef libpaths pkgid v = do
  st <- get
  when (not $ v `S.member` alreadySeen st) $ do
    m <- getModuleOf libpaths v
    -- getModuleOf may update the state, so we need to refresh it
    st' <- get
    let dependencies = maybe S.empty id (M.lookup v (modDeps m))
    put st' {alreadySeen = S.insert v (alreadySeen st')}
    S.foldl' (\a x -> a >> addDef libpaths pkgid x) (return ()) dependencies

    -- addDef _definitely_ updates the state, so refresh once again
    st'' <- get
    let Name cmnt _ = v
        defs' =
          maybe (defs st'')
                (\body -> defs st'' . newVar True (internalVar v cmnt) body)
                (M.lookup v (modDefs m))
    put st'' {defs = defs'}
