{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------------------------
-- |
-- Module: Licensor
-- Description:
--
--
--
----------------------------------------------------------------------

module Licensor
  ( LiLicense(..)
  , LiPackage(..)
  , getDependencies
  , getLicenses
  , getPackage
  , orderPackagesByLicense
  , version
  )
  where

-- base
import qualified Control.Exception as Exception
import Control.Monad (unless)
import Data.Version (Version)

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

-- licensor
import qualified Paths_licensor

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
getDependencies = do
  eitherDeps <-
    Exception.try $ readProcess "stack" ["ls", "dependencies", "--separator", "-"] ""

  case eitherDeps of
    Left (_ :: IOError) ->
      return Nothing

    Right deps ->
      return $ fmap Set.fromList $ sequence $ fmap simpleParse (lines deps)


getLicenses :: IO (Maybe [(PackageName, License)])
getLicenses = do
  eitherDeps <-
    Exception.try $ readProcess "stack" ["ls", "dependencies", "--license"] ""

  case eitherDeps of
    Left (_ :: IOError) ->
      return Nothing

    Right deps ->
      return $ sequence $ fmap toNameLicense (lines deps)
  where
    toNameLicense dep =
      case words dep of
        [name, license] ->
          (,) <$> simpleParse name <*> simpleParse license

        _ ->
          Nothing


-- |
--
--

getPackageLicense
  :: Bool
  -> PackageIdentifier
  -> [(PackageName, License)]
  -> IO (Maybe LiLicense)
getPackageLicense quiet p@PackageIdentifier{..} licenses = do
  unless quiet (putStr $ display p ++ "...")
  case lookup pkgName licenses of
    Just license -> do
      unless quiet (putStrLn $ display license)
      return $ Just (LiLicense license)
    Nothing ->
      return Nothing
      -- PackageDescription{license} <- getPackageDescription file


-- |
--
--

orderPackagesByLicense
  :: Bool
  -> Maybe PackageIdentifier
  -> [(PackageName, License)]
  -> Set PackageIdentifier
  -> IO (Map LiLicense (Set PackageIdentifier), Set PackageIdentifier)
orderPackagesByLicense quiet maybeP licenses =
  let
    cond =
      maybe (const False) (==) maybeP

    insertPackage package orderedPackages' = do
      maybeLicense <- getPackageLicense quiet package licenses

      (orderedPackages, failed) <- orderedPackages'
      return $
        if cond package
          then
            (orderedPackages, failed)
          else
            case maybeLicense of
              Nothing ->
                ( orderedPackages, Set.insert package failed
                )

              Just license ->
                ( Map.insertWith
                    Set.union
                    license
                    (Set.singleton package)
                    orderedPackages
                , failed
                )
  in
    foldr insertPackage (pure (mempty, mempty))


-- |
--
--

version :: Version
version =
  Paths_licensor.version
