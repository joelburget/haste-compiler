{-# LANGUAGE CPP #-}
module Haste.Compile (compileMain, compileDep) where

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
import FastString
import Module (packageIdString, moduleNameFS)
import System.Environment (getArgs)
import Control.Monad (when, forM_)
import Haste
import Haste.Args
import Haste.Config
import Haste.CommonJS
import Haste.Opts
import Haste.Environment
import Haste.Version
import System.IO
import System.Exit (exitFailure)
import Data.Version
import Data.List
import Data.JSTarget.AST
import qualified Control.Shell as Sh
import Name (getOccString)
import TcEvidence (HsWrapper(WpHole))
import NameSet (emptyNameSet)
import qualified Data.ByteString.Lazy.Char8 as B8
import HsExpr
import BasicTypes
import RdrName (mkVarUnqual)
import Outputable


-- | Do everything required to get a list of STG bindings out of a module.
prepare :: GhcMonad m => DynFlags -> ModSummary -> m ([StgBinding], ModuleName)
prepare dynflags modSummary = do
  env <- getSession
  let mod = ms_mod modSummary
      name = moduleName mod
  -- pgm1 <- parseModule modSummary
  -- liftIO $ putStrLn "before injectDefns"
  -- let pgm2 = injectDefns name pgm1
  -- liftIO $ putStrLn "after injectDefns"
  pgm <- parseModule modSummary
    -- >>= (\x -> return $ injectDefns name x)
    >>= typecheckModule
    >>= desugarModule
    >>= liftIO . hscSimplify env . coreModule
    >>= liftIO . tidyProgram env
    >>= prepPgm env . fst
#if __GLASGOW_HASKELL__ >= 707
    >>= liftIO . coreToStg dynflags mod
#else
    >>= liftIO . coreToStg dynflags
#endif
  -- liftIO $ putStrLn "after pgm"
  -- liftIO $ do
  --     putStrLn "here"
  --     printForUser dynflags stdout neverQualify (ppr (pm_parsed_source pgm2))
  --     putStrLn "there"
  return (pgm, name)
  where
    injectDefns :: ModuleName -> ParsedModule -> ParsedModule
    injectDefns modname (ParsedModule sum locSrc files) =
        let loc = getLoc locSrc
            hsMod = unLoc locSrc
            locatedDecls = hsmodDecls hsMod
            modNameStr = moduleNameString modname
            modNameFS = moduleNameFS modname
            newDecls =
                [ loaderDecl modNameFS (fsLit "onHotLoad")
                , loaderDecl modNameFS (fsLit "onHotUnload")
                ]
            locatedNewDecls = map noLoc newDecls
            decls' = locatedNewDecls ++ locatedDecls
            hsMod' = hsMod { hsmodDecls = decls' }

        in ParsedModule sum (L loc hsMod') files

    prepPgm env tidy = liftIO $
      corePrepPgm dynflags env (cg_binds tidy) (cg_tycons tidy)

loaderDecl :: FastString -> FastString -> HsDecl RdrName
loaderDecl modname funcname =
    let funcname' = mkVarUnqual funcname
        jsFuncName = mkVarUnqual (concatFS [fsLit "js_", funcname])
    in ValD $ FunBind
        (noLoc funcname')
        False
        (MG
            [noLoc (Match [] Nothing (GRHSs
                [ noLoc (GRHS []
                              (noLoc (HsApp (noLoc (HsVar jsFuncName))
                                            (noLoc (HsLit (HsString modname))))))
                ]
                EmptyLocalBinds))
            ]
            []
            placeHolderType
            Generated
        )
        WpHole
        emptyNameSet
        Nothing

-- onHotLoad :: Ptr (IO ()) -> IO ()
-- onHotLoad = js_onHotLoad "modulename"

-- onHotUnload :: Ptr (IO ()) -> IO ()
-- onHotUnload = js_onHotUnload "modulename"

compile :: GhcMonad m
        => FilePath
        -> DynFlags
        -> ModSummary
        -> m (B8.ByteString, String, ModuleName)
compile path dynflags modSummary = do
    -- XXX boot
    -- XXX we're not actually compiling jsmods anymore
    let boot = case ms_hsc_src modSummary of
                 HsBootFile -> True
                 _          -> False
    (pgm, name) <- prepare dynflags modSummary

    let mod = ms_mod modSummary
        pkgid = showPpr dynflags $ modulePackageId mod
        cfg = def {showOutputable = showPpr dynflags}
        theCode = generate cfg pkgid name pgm

    Just modInfo <- getModuleInfo mod
    let exportNames = map getOccString (modInfoExports modInfo)
    return (commonJs path theCode exportNames, pkgid, name)


compileMain :: GhcMonad m
            => FilePath
            -> DynFlags
            -> ModSummary
            -> m B8.ByteString
compileMain basepath dynflags modSummary = do
    (str, _, _) <- compile basepath dynflags modSummary
    return str

compileDep :: GhcMonad m => FilePath -> DynFlags -> ModSummary -> m ()
compileDep basepath flags summary = do
    (str, pkgid, modname) <- compile basepath flags summary
    -- liftIO $ do
    --     putStrLn "compiling dep:"
    --     putStrLn $ B8.unpack str
    --     putStrLn pkgid
    --     putStrLn $ moduleNameString modname
    liftIO $ writeCommonJS basepath pkgid (moduleNameString modname) str
