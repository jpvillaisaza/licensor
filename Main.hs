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
import qualified System.Exit as Exit

-- Cabal
import Distribution.PackageDescription
import Distribution.Text

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- |
--
--

main :: IO ()
main = do
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
