{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
  where

-- licensor
import Licensor

-- base
import Control.Monad
import Data.List
import Data.Monoid ((<>))
import qualified Data.Version as Version
import System.Environment
import qualified System.Exit as Exit

-- Cabal
import Distribution.PackageDescription
import Distribution.Text

-- cmdargs
import System.Console.CmdArgs

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- |
--
--

data LiArgs =
  LiArgs
    {
    }
  deriving (Data)


-- |
--
--

liArgs :: String -> Mode (CmdArgs LiArgs)
liArgs s =
  cmdArgsMode $
    LiArgs
      {
      }
  &= program s
  &= summary (unwords ["licensor", Version.showVersion version])


-- |
--
--

main :: IO ()
main = do
  LiArgs <- cmdArgsRun . liArgs =<< getProgName

  maybePackage <- getPackage

  pid <-
    case maybePackage of
      Nothing ->
        Exit.die "Error: No Cabal file found."

      Just PackageDescription{..} -> do
        putStrLn $
          "Package: "
            <> display package
            <> " ("
            <> "License: "
            <> display license
            <> ")"
        return package

  maybeDependencies <- getDependencies

  case maybeDependencies of
    Nothing ->
      Exit.die "Error: ..."

    Just dependencies -> do
      dependenciesByLicense <-
        fmap (Set.map display) <$> orderPackagesByLicense pid dependencies

      forM_ (Map.keys dependenciesByLicense) $
        \license ->
          let
            n = dependenciesByLicense Map.! license
          in do
            putStrLn "-----"
            putStrLn $
              show (Set.size n)
                <> (if Set.size n == 1 then " package " else " packages ")
                <> "licensed under "
                <> display license
                <> ": "
                <> intercalate ", " (Set.toList n)
