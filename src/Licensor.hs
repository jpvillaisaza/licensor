{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Licensor
  ( LiLicense(..)
  , LiPackage(..)
  , getDependencies
  , getPackage
  , orderPackagesByLicense
  , version
  )
  where

-- licensor
import qualified Paths_licensor as Paths

-- base
import Data.Monoid ((<>))
import Data.Version (Version)
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

newtype LiLicense = LiLicense { getLicense :: License }
  deriving (Eq, Read, Show, Text)


-- |
--
--

instance Ord LiLicense where
  compare =
    comparing display


-- |
--
--

data LiPackage =
  LiPackage
    { liPackageId :: PackageIdentifier
    , liPackageDependencies :: Set LiPackage
    , liPackageLicense :: License
    }


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

getPackageLicense :: PackageIdentifier -> IO LiLicense
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
  return (LiLicense license)


-- |
--
--

orderPackagesByLicense
  :: PackageIdentifier
  -> Set PackageIdentifier
  -> IO (Map LiLicense (Set PackageIdentifier))
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


-- |
--
--

version :: Version
version =
  Paths.version
