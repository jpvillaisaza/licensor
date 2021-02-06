----------------------------------------------------------------------
-- |
-- Module: Main
-- Description:
--
--
--
----------------------------------------------------------------------

module Main
  ( main
  )
  where

-- Cabal
import Distribution.PackageDescription
import Distribution.Text

-- base
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (foldl', intercalate)
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- directory
import System.Directory (doesFileExist)

-- licensor
import Licensor


prettyUsageInfo :: String -> String
prettyUsageInfo progName =
  "usage: " <> usageInfo progName optDescrs


prettyVersion :: String -> String
prettyVersion progName =
  unwords [progName, showVersion version]


optDescrs :: [OptDescr (Bool -> IO Bool)]
optDescrs =
  [ Option ['q', 's'] ["quiet", "silent"]
      (NoArg (\_ -> pure False))
      "Quiet/silent mode"
  , Option ['h'] ["help"]
      (NoArg (\_ -> exitWith prettyUsageInfo))
      "Display help message"
  , Option ['V'] ["version"]
      (NoArg (\_ -> exitWith prettyVersion))
       "Print version information"
   ]
   where
     exitWith f = do
       progName <- getProgName
       hPutStrLn stdout (f progName)
       exitSuccess


main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case getOpt' Permute optDescrs args of
    (opts, [], [], []) -> do
      verbose <- foldl' (>>=) (pure True) opts
      main' verbose
    (_, command:_, _, _) -> do
      hPutStrLn stderr ("unknown command: " <> command)
      hPutStrLn stderr (prettyUsageInfo progName)
      exitFailure
    (_, _, option:_, _) -> do
      hPutStrLn stderr ("unknown option: " <> option)
      hPutStrLn stderr (prettyUsageInfo progName)
      exitFailure
    (_, _, _, _) -> do
      hPutStrLn stderr (prettyUsageInfo progName)
      exitFailure


main' :: Bool -> IO ()
main' verbose = do
  let silent = not verbose

  maybePackage <- getPackage

  pid <-
    case maybePackage of
      Nothing -> do
        stack <- doesFileExist "stack.yaml"
        if stack
          then do
            putStrLn "Found stack.yaml..."
            pure Nothing
          else
            die "Error: No stack.yaml file found."

      Just pd -> do
        putStrLn $
          "Package: "
            <> display (package pd)
            <> " ("
            <> "License: "
            <> display (license pd)
            <> ")"
        pure (Just $ package pd)

  maybeDependencies <- getDependencies
  maybeLicenses <- getLicenses

  case (maybeDependencies, maybeLicenses) of
    (Just dependencies, Just licenses) -> do
      (dependenciesByLicense', failed) <-
        orderPackagesByLicense silent pid licenses dependencies

      let dependenciesByLicense = fmap (Set.map display) dependenciesByLicense'

      for_ (Map.keys dependenciesByLicense) $
        \li ->
          let
            n = dependenciesByLicense Map.! li
          in do
            putStrLn "-----"
            putStrLn $
              show (Set.size n)
                <> (if Set.size n == 1 then " package " else " packages ")
                <> "licensed under "
                <> display li
                <> ": "
                <> intercalate ", " (Set.toList n)

      unless (null failed) $ do
        putStr "Failed: "
        print failed
    _ ->
      die "Error: Unable to run 'stack ls dependencies'"
