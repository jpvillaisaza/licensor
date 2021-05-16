{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
  , getPackageLicenseFiles
  , orderPackagesByLicense
  , version
  )
  where

-- base
import qualified Control.Exception as Exception
import Control.Monad (unless)
import Data.Traversable (for)
import Data.Version (Version)

-- bytestring
import qualified Data.ByteString.Lazy as LBS

-- Cabal
import Distribution.License (License)
import Distribution.Package (PackageIdentifier(..), PackageName, unPackageName)
import Distribution.PackageDescription
  (PackageDescription, licenseFiles, packageDescription)
import Distribution.PackageDescription.Parsec
  (parseGenericPackageDescriptionMaybe, readGenericPackageDescription)
import Distribution.Pretty (Pretty)
import Distribution.Simple.Utils (comparing, findPackageDesc)
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (silent)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- directory
import System.Directory (getCurrentDirectory)

-- http-client
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)

-- http-client-tls
import Network.HTTP.Client.TLS (getGlobalManager)

-- licensor
import qualified Paths_licensor

-- process
import System.Process (readProcess)

-- tar
import qualified Codec.Archive.Tar as Tar

-- temporary
import qualified System.IO.Temp as Temp

-- zlib
import qualified Codec.Compression.GZip as GZip

-- |
--
--

newtype LiLicense = LiLicense { getLicense :: License }
  deriving (Eq, Read, Show, Pretty)


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
    >>= either (const (pure Nothing)) (fmap Just)


-- |
--
--

getPackageDescription :: FilePath -> IO PackageDescription
getPackageDescription =
  fmap packageDescription . readGenericPackageDescription silent


-- |
--
--

getDependencies :: IO (Maybe (Set PackageIdentifier))
getDependencies = do
  eitherDeps <-
    Exception.try $ readProcess "stack" ["ls", "dependencies", "--separator", "-"] ""

  case eitherDeps of
    Left (_ :: IOError) ->
      pure Nothing

    Right deps ->
      pure $ Set.fromList <$> traverse simpleParse (lines deps)


getLicenses :: IO (Maybe [(PackageName, License)])
getLicenses = do
  eitherDeps <-
    Exception.try $ readProcess "stack" ["ls", "dependencies", "--license"] ""

  case eitherDeps of
    Left (_ :: IOError) ->
      pure Nothing

    Right deps ->
      pure $ traverse toNameLicense (lines deps)
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
getPackageLicense quiet packageIdentifier licenses = do
  unless quiet (putStr $ display packageIdentifier ++ "...")
  case lookup (pkgName packageIdentifier) licenses of
    Just license -> do
      unless quiet (putStrLn $ display license)
      pure $ Just (LiLicense license)
    Nothing ->
      pure Nothing


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
      pure $
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


getPackageLicenseFiles :: PackageIdentifier -> IO (Maybe [String])
getPackageLicenseFiles packageIdentifier =
  getPackageLicenseFiles'
    (unPackageName (pkgName packageIdentifier))
    (display packageIdentifier)

getPackageLicenseFiles' :: String -> String -> IO (Maybe [String])
getPackageLicenseFiles' packageName packageId = do
  putStrLn ""
  putStrLn ("### " <> packageId)
  putStrLn ""
  manager <- getGlobalManager
  genPkgDescrReq <- parseRequest (mkRequest (packageName <> ".cabal"))
  genPkgDescr <- LBS.toStrict . responseBody <$> httpLbs genPkgDescrReq manager
  let mGenPkgDescr = parseGenericPackageDescriptionMaybe genPkgDescr
  case licenseFiles . packageDescription <$> mGenPkgDescr of
    Just files -> do
      pkgReq <- parseRequest (mkRequest (packageId <> ".tar.gz"))
      pkg <- responseBody <$> httpLbs pkgReq manager
      let entries = Tar.read (GZip.decompress pkg)
      pkgLicenseFiles <-
        Temp.withSystemTempDirectory packageId $ \tmpDir -> do
          Tar.unpack tmpDir entries
          for files $ \file -> do
            contents <- readFile (tmpDir <> "/" <> packageId <> "/" <> file)
            putStrLn (file <> ":")
            putStrLn ""
            putStrLn "```"
            putStrLn contents
            putStrLn "```"
            pure file
      pure (Just pkgLicenseFiles)
    Nothing ->
      pure Nothing
  where
    mkRequest r =
      "GET https://hackage.haskell.org/package/" <> packageId <> "/" <> r
