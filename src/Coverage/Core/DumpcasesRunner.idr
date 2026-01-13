||| Dumpcases Runner - Backend-agnostic --dumpcases execution
|||
||| Provides an interface for running `idris2 --dumpcases` with different
||| code generators (Chez, RefC, EVM).
|||
||| Usage:
|||   result <- runDumpcasesWithBackend ChezBackend projectDir ipkgName
|||   result <- runDumpcasesWithBackend RefcBackend projectDir ipkgName
|||   result <- runDumpcasesWithBackend EvmBackend projectDir ipkgName
module Coverage.Core.DumpcasesRunner

import Data.List
import Data.String
import System
import System.File

%default covering

-- =============================================================================
-- Backend Configuration
-- =============================================================================

||| Code generator backend type
public export
data Backend
  = ChezBackend    -- Default Chez Scheme backend
  | RefcBackend    -- RefC backend (for WASM/DFX)
  | EvmBackend     -- EVM backend (for Ethereum)
  | CustomBackend String  -- Custom codegen name

public export
Show Backend where
  show ChezBackend = "chez"
  show RefcBackend = "refc"
  show EvmBackend = "evm"
  show (CustomBackend name) = name

||| Get the --codegen flag for a backend
||| Chez doesn't need explicit --codegen flag
codegenFlag : Backend -> String
codegenFlag ChezBackend = ""
codegenFlag RefcBackend = "--codegen refc "
codegenFlag EvmBackend = "--codegen evm "
codegenFlag (CustomBackend name) = "--codegen " ++ name ++ " "

-- =============================================================================
-- Build Command Generation
-- =============================================================================

||| Generate idris2 --dumpcases build command
|||
||| @backend - Which code generator to use
||| @projectDir - Project root directory
||| @ipkgName - Package file name (e.g., "myproject.ipkg")
||| @outputPath - Where to write dumpcases output
public export
buildDumpcasesCommand : Backend -> String -> String -> String -> String
buildDumpcasesCommand backend projectDir ipkgName outputPath =
  "cd " ++ projectDir ++ " && idris2 --dumpcases " ++ outputPath ++ " " ++
  codegenFlag backend ++ "--build " ++ ipkgName ++ " 2>&1"

||| Default output path for temporary dumpcases file
public export
defaultOutputPath : String
defaultOutputPath = "/tmp/idris2_dumpcases_output.txt"

||| Backend-specific output path
public export
backendOutputPath : Backend -> String
backendOutputPath ChezBackend = "/tmp/idris2_dumpcases_chez.txt"
backendOutputPath RefcBackend = "/tmp/idris2_dumpcases_refc.txt"
backendOutputPath EvmBackend = "/tmp/idris2_dumpcases_evm.txt"
backendOutputPath (CustomBackend name) = "/tmp/idris2_dumpcases_" ++ name ++ ".txt"

-- =============================================================================
-- Dumpcases Execution
-- =============================================================================

||| Run idris2 --dumpcases with specified backend
|||
||| @backend - Which code generator to use
||| @projectDir - Project root directory (containing .ipkg)
||| @ipkgName - Package file name
||| @outputPath - Where to write dumpcases output
|||
||| Returns: Either error message or dumpcases content
public export
runDumpcases : (backend : Backend)
            -> (projectDir : String)
            -> (ipkgName : String)
            -> (outputPath : String)
            -> IO (Either String String)
runDumpcases backend projectDir ipkgName outputPath = do
  let cmd = buildDumpcasesCommand backend projectDir ipkgName outputPath
  exitCode <- system cmd
  if exitCode /= 0
    then pure $ Left $ "Build failed with exit code " ++ show exitCode
    else do
      result <- readFile outputPath
      case result of
        Left err => pure $ Left $ "Failed to read dumpcases output: " ++ show err
        Right content =>
          if null (trim content)
            then pure $ Left "No dumpcases output generated (build may have failed)"
            else pure $ Right content

||| Run dumpcases with default output path for backend
public export
runDumpcasesDefault : Backend -> String -> String -> IO (Either String String)
runDumpcasesDefault backend projectDir ipkgName =
  runDumpcases backend projectDir ipkgName (backendOutputPath backend)

-- =============================================================================
-- Convenience Functions for Each Backend
-- =============================================================================

||| Run dumpcases with Chez backend (default)
public export
runDumpcasesChez : String -> String -> IO (Either String String)
runDumpcasesChez = runDumpcasesDefault ChezBackend

||| Run dumpcases with RefC backend (for DFX/WASM)
public export
runDumpcasesRefc : String -> String -> IO (Either String String)
runDumpcasesRefc = runDumpcasesDefault RefcBackend

||| Run dumpcases with EVM backend
public export
runDumpcasesEvm : String -> String -> IO (Either String String)
runDumpcasesEvm = runDumpcasesDefault EvmBackend

-- =============================================================================
-- Pack Build Support
-- =============================================================================

||| Run dumpcases using pack build (for better package resolution)
|||
||| This is useful when the project has complex dependencies that
||| require pack's package resolution.
public export
runDumpcasesWithPack : (backend : Backend)
                    -> (projectDir : String)
                    -> (ipkgName : String)
                    -> (outputPath : String)
                    -> IO (Either String String)
runDumpcasesWithPack backend projectDir ipkgName outputPath = do
  -- Create a temporary ipkg with opts for --dumpcases
  let tempIpkgContent = generateTempIpkg backend ipkgName outputPath
  let tempIpkgPath = projectDir ++ "/_dumpcases_temp.ipkg"

  -- Write temp ipkg
  writeResult <- writeFile tempIpkgPath tempIpkgContent
  case writeResult of
    Left err => pure $ Left $ "Failed to write temp ipkg: " ++ show err
    Right () => do
      -- Run pack build
      let cmd = "cd " ++ projectDir ++ " && pack build _dumpcases_temp.ipkg 2>&1"
      exitCode <- system cmd

      -- Clean up temp ipkg
      _ <- system $ "rm -f " ++ tempIpkgPath

      if exitCode /= 0
        then pure $ Left $ "Pack build failed with exit code " ++ show exitCode
        else do
          result <- readFile outputPath
          case result of
            Left err => pure $ Left $ "Failed to read dumpcases output: " ++ show err
            Right content =>
              if null (trim content)
                then pure $ Left "No dumpcases output generated"
                else pure $ Right content
  where
    generateTempIpkg : Backend -> String -> String -> String
    generateTempIpkg be origIpkg out =
      let cg = case be of
                 ChezBackend => ""
                 RefcBackend => " --codegen refc"
                 EvmBackend => " --codegen evm"
                 CustomBackend name => " --codegen " ++ name
          -- Strip .ipkg extension to get package name
          pkgName = if isSuffixOf ".ipkg" origIpkg
                      then substr 0 (length origIpkg `minus` 5) origIpkg
                      else origIpkg
      in unlines
        [ "package _dumpcases_temp"
        , "opts = \"--dumpcases " ++ out ++ cg ++ "\""
        , "depends = " ++ pkgName
        , "main = Main"
        ]
