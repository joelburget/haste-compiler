{-# LANGUAGE OverloadedStrings #-}
module Haste.CommonJS (writeCommonJS, commonJs) where

import Module (moduleNameSlashes, mkModuleName)
import Control.Shell
import Data.List (intersperse)
import Haste.Config
import Haste.Module
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Control.Applicative
import Data.JSTarget
import Data.JSTarget.PP hiding (put)
import qualified Data.ByteString.Lazy as B
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid
import System.IO (hPutStrLn, stderr)
import Encoding

import Debug.Trace

data ImportName = ImportName
    { importJsName :: Builder
    , importHsName :: Builder
    , importPath :: FilePath
    }

data ExportName = ExportName
    { exportJsName :: Builder
    , exportHsName :: Builder
    }

data DepState = DepState
    { alreadySeen :: !(S.Set Name)
    , defs :: AST Stm -> AST Stm
    }

instance Show DepState where
    show (DepState _ f) = show (f nullRet)

type DepM a = EitherT Name (State DepState) a

initState :: DepState
initState = DepState
    { alreadySeen = S.empty
    , defs = id
    }

defineModule :: DepM a -> AST Stm
defineModule m =
    let res = runState (runEitherT m) initState
    in case res of
           (Right _, st) -> defs st nullRet
           (Left (Name f (Just (p, m))), _) -> error $
               "Unable to locate function `" ++ f ++
               "' in module `" ++ m ++ "'!"

require :: ImportName -> Builder
require (ImportName jsName hsName path) =
    "var " <> hsName <> " = require('" <> fromString path <> "')['" <> jsName <> "'];"

lineSep :: [Builder] -> Builder
lineSep = mconcat . intersperse "\n"

requires :: [ImportName] -> Builder
requires = lineSep . fmap require

expt :: ExportName -> Builder
expt (ExportName jsName hsName) = "'" <> jsName <> "': " <> hsName <> ","

exports :: [ExportName] -> Builder
exports names = lineSep
    [ "module.exports = {"
    , lineSep $ fmap expt names
    , "};"
    ]

getHsName :: Name -> PP Builder
getHsName name = buildFinalName <$> finalNameFor name

absPkgPath :: FilePath -> String -> String -> Bool -> FilePath
absPkgPath basepath pkg mod boot = basepath </> relPkgPath pkg mod boot

relPkgPath :: String -> String -> Bool -> FilePath
relPkgPath pkgid modname boot =
    flip addExtension (commonJsExt boot) $
        pkgid </> moduleNameSlashes (mkModuleName modname)

interfaceInfo :: FilePath -> S.Set String -> Module -> PP ([ImportName], [ExportName])
interfaceInfo basepath exportNames mod =
    let dependencies :: [Name]
        dependencies =
            let deps = S.unions (M.elems (modDeps mod))
                -- filter out dependencies which aren't actually to
                -- external files
                externalDeps = S.filter
                    -- probably only the second of these filters is
                    -- necesary?
                    (\(Name _ pkgMod) ->
                        pkgMod /= Just (modPackageId mod, modName mod) &&
                        pkgMod /= Just ("main", modName mod))
                    deps
            in S.toList externalDeps

        importInfo :: Name -> PP ImportName
        importInfo name@(Name subName (Just (pkg, mod))) = ImportName
            <$> pure (zEncode subName)
            <*> getHsName name
            <*> pure (absPkgPath basepath pkg mod False)

        definitions :: [Name]
        -- -- XXX figure out which things to actually export
        definitions = filter
            -- (\(Name name pkgInfo) -> pkgInfo /= (Just ("main", ":Main")) && name `S.member` exportNames)
            (\(Name name pkgInfo) -> pkgInfo /= Just ("main", ":Main"))
            (M.keys (modDefs mod))

        exportInfo :: Name -> PP ExportName
        exportInfo name@(Name subName _) = ExportName
            <$> pure (zEncode subName)
            <*> getHsName name

    in (,) <$> mapM importInfo dependencies <*> mapM exportInfo definitions

zEncode :: String -> Builder
zEncode = fromString . zEncodeString

assembleProg :: [ImportName] -> Builder -> [ExportName] -> String -> Builder
assembleProg fileImports progText fileExports modname = let nm = fromString modname in lineSep
    [ requires fileImports
    , progText

    -- make sure this is *after* defs since it may use them
    , "if (module.hot) {"
    , "    console.log('hot loading " <> nm <> "');"
    , "    module.hot.accept();"
    ,      writeHotLoading fileExports
    , "}"

    , exports fileExports
    ]

writeHotLoading :: [ExportName] -> Builder
writeHotLoading (ExportName jsName hsName:xs) =
    let this = case toLazyByteString jsName of
                   "loadHandler" -> "B(A(" <> hsName <> ", [0]));"
                   "unloadHandler" -> "module.hot.dispose(function() { B(A(" <> hsName <> ", [0])); });"
                   _ -> ""
    in this <> writeHotLoading xs
writeHotLoading [] = ""

-- | The file extension to use for modules.
commonJsExt :: Bool -> String
commonJsExt boot = if boot then "boot.js" else "js"

addDef :: Module -> Name -> DepM ()
addDef m v@(Name cmnt _) = do

    st <- get
    unless (v `S.member` alreadySeen st) $ do
        let dependencies = M.findWithDefault S.empty v (modDeps m)
        put st{alreadySeen = S.insert v (alreadySeen st)}
        mapM_ (addDef m) (S.toList dependencies)

        st' <- get
        -- let defs' = defs st' . newVar True (internalVar v cmnt) ast
        let defs' = maybe
                (defs st')
                (\body -> defs st' . newVar True (internalVar v cmnt) body)
                (M.lookup v (modDefs m))
        put st'{defs = defs'}

pretty' :: PPOpts
        -> AST Stm
        -> PP ([ImportName], [ExportName])
        -> (Builder, ([ImportName], [ExportName]))
pretty' opts (AST ast js) m = runPP opts js (pp ast >> m)

writeCommonJS :: FilePath -> String -> String -> B.ByteString -> IO ()
writeCommonJS basepath pkgid modname bStr =
    fromRight "writeModule" . shell $ do
        let path = absPkgPath basepath pkgid modname False
        mkdir True (takeDirectory path)
        liftIO $ B.writeFile path bStr

commonJs :: FilePath -> Module -> [String] -> B.ByteString
commonJs basepath m@(Module pkgid modname deps defs) exportNames =
    -- how ":Main" gets in here, I don't know
    let defsAst = defineModule $ mapM_ (addDef m) (M.keys defs)
        (progText, (fileImports, fileExports)) = pretty' debugPPOpts defsAst $
            interfaceInfo basepath (S.fromList exportNames) m

    in toLazyByteString $ assembleProg fileImports progText fileExports modname
