{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
  where

-- base
import Control.Monad
import Data.List
import Data.Monoid ((<>))
import qualified System.Exit as Exit
import System.IO

-- Cabal
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Verbosity

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- directory
import System.Directory

-- HTTP
import Network.HTTP
  ( getRequest
  , getResponseBody
  , simpleHTTP
  )

-- process
import System.Process


-- |
--
--

newtype License' = License' { _getLicense :: License }
  deriving (Eq, Read, Show, Text)


-- |
--
--

instance Ord License' where
  compare =
    comparing display


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


-- |
--
--

getPackage :: IO (Maybe PackageDescription)
getPackage = do
  currentDirectory <- getCurrentDirectory
  fmap getPackageDescription <$> findPackageDesc currentDirectory
    >>= either (const (return Nothing)) (fmap Just)


-- |
--
--

getPackageDescription :: FilePath -> IO PackageDescription
getPackageDescription =
  fmap packageDescription . readPackageDescription silent


-- |
--
--

getDependencies :: IO (Maybe (Set PackageIdentifier))
getDependencies =
  fmap Set.fromList . sequence . fmap simpleParse . lines
    <$> readProcess "stack" ["list-dependencies", "--separator", "-"] ""


-- |
--
--

getPackageLicense :: PackageIdentifier -> IO License'
getPackageLicense p@PackageIdentifier{..} = do
  let
    url =
      "http://hackage.haskell.org/package/"
        <> display p
        <> "/"
        <> unPackageName pkgName
        <> ".cabal"
  pd <- simpleHTTP (getRequest url) >>= getResponseBody
  (file, handle) <- openTempFile "/tmp" "licensor"
  hClose handle
  writeFile file pd
  PackageDescription{license} <- getPackageDescription file
  hClose handle
  removeFile file
  return (License' license)


-- |
--
--

orderPackagesByLicense
  :: PackageIdentifier
  -> Set PackageIdentifier
  -> IO (Map License' (Set PackageIdentifier))
orderPackagesByLicense p =
  let
    insertPackage package orderedPackages' = do
      license <- getPackageLicense package
      orderedPackages <- orderedPackages'
      return $
        if p == package
          then
            orderedPackages
          else
            Map.insertWith
              Set.union
              license
              (Set.singleton package)
              orderedPackages
  in
    foldr insertPackage (pure mempty)
