{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Bosh.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Map
import Data.Scientific
import Data.Text
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Casing

data Manifest = Manifest { name           :: Text
                         , directorUUID   :: Maybe Text
                         , features       :: Maybe Features
                         , releases       :: [Release]
                         , stemcells      :: [Stemcell]
                         , update         :: Update
                         , instanceGroups :: [InstanceGroup]
                         , addons         :: Maybe [Addon] -- According to docs this is required, but it is not
                         , properties     :: Maybe (Map Text Value)
                         , variables      :: Maybe [Variable] -- According to docs this is required, but it is not
                         , tags           :: Maybe (Map Text Text)
                         }  deriving (Show, Eq, Generic)

data Release = Release { releaseName       :: Text
                       , releaseVersion    :: Text
                       , releaseUrl        :: Maybe Text
                       , releaseSha1       :: Maybe Text
                       , releaseStemcell   :: Maybe ReleaseStemcell
                       , releaseExportedFrom :: Maybe ([ReleaseStemcell])
                       }  deriving (Show, Eq, Generic)

data InstanceGroup =
  InstanceGroup { igName               :: Text
                , igAzs                :: [Text]
                , igInstances          :: Int
                , igJobs               :: [Job]
                , igVmType             :: Text
                , igVmExtensions       :: Maybe [Text]
                , igVmResources        :: Maybe VmResources
                , igStemcell           :: Text
                , igPersistentDisk     :: Maybe Int
                , igPersistentDiskType :: Maybe Text
                , igNetworks           :: [Network]
                , igUpdate             :: Maybe Update
                , igMigratedFrom       :: Maybe [Text]
                , igLifecycle          :: Maybe Text
                , igProperties         :: Maybe Value
                , igEnv                :: Maybe InstanceGroupEnv
                }  deriving (Show, Eq, Generic)

data Stemcell = Stemcell { stemcellAlias   :: Text
                         , stemcellOs      :: Maybe Text
                         , stemcellVersion :: Text
                         , stemcellName    :: Maybe Text
                         }  deriving (Show, Eq, Generic)

data Update = Update { updateCanaries        :: Int
                     , updateMaxInFlight     :: Value
                     , updateCanaryWatchTime :: Value
                     , updateUpdateWatchTime :: Value
                     , updateSerial          :: Maybe Bool
                     , updateVmStrategy      :: Maybe Text
                     }  deriving (Show, Eq, Generic)


data Features = Features { featuresConvergeVariables    :: Maybe Bool
                         , featuresRandomizeAZPlacement :: Maybe Bool
                         , featuresUseDNSAddresses      :: Maybe Bool
                         , featuresUseTmpfsJobConfig    :: Maybe Bool
                         , featuresUseShortDNSAddresses :: Maybe Bool
                         }  deriving (Show, Eq, Generic)

data Addon = Addon { addonName    :: Text
                   , addonJobs    :: [Job]
                   , addonInclude :: Maybe Value
                   , addonExclude :: Maybe Value
                   }  deriving (Show, Eq, Generic)

data Variable = Variable { varName       :: Text
                         , varType       :: Text -- can be made enum
                         , varUpdateMode :: Maybe Text -- can be made enum
                         , varOptions    :: Maybe (Map Text Value)
                         }  deriving (Show, Eq, Generic)

data ReleaseStemcell = ReleaseStemcell { rsOS      :: Text
                                       , rsVersion :: Text
                                       }  deriving (Show, Eq, Generic)

data Job = Job { jobName       :: Text
               , jobRelease    :: Text
               , jobConsumes   :: Maybe (Map Text Consume)
               , jobProvides   :: Maybe (Map Text Provide)
               , jobProperties :: Maybe Value
               }  deriving (Show, Eq, Generic)

data Consume = ConsumeLink ConsumeFrom
             | ConsumeNil
  deriving (Show, Eq)

instance ToJSON Consume where
  toJSON ConsumeNil = Null
  toJSON (ConsumeLink l) = toJSON l

instance FromJSON Consume where
  parseJSON Null = pure ConsumeNil
  parseJSON o = ConsumeLink <$> parseJSON o

data ConsumeFrom = ConsumeFrom { consumeFrom       :: Text
                               , consumeDeployment :: Maybe Text
                               , consumeNetwork    :: Maybe Text
                               , consumeIpAddress  :: Maybe Bool
                               }  deriving (Show, Eq, Generic)

data Provide = ProvideLink ProvideAs
             | ProvideNil
  deriving (Show, Eq)

instance ToJSON Provide where
  toJSON ProvideNil = Null
  toJSON (ProvideLink l) = toJSON l

instance FromJSON Provide where
  parseJSON Null = pure ProvideNil
  parseJSON o = ProvideLink <$> parseJSON o

data ProvideAs = ProvideAs { provideAs     :: Text
                           , provideShared :: Maybe Bool
                           }  deriving (Show, Eq, Generic)

data VmResources = VmResources { vmrCPU               :: Int
                               , vmrRAM               :: Int
                               , vmrEphemeralDiskSize :: Int
                               }  deriving (Show, Eq, Generic)

data Network = Network { networkName      :: Text
                       , networkStaticIps :: Maybe [Text]
                       , networkDefault   :: Maybe [Text]
                       }  deriving (Show, Eq, Generic)

data IntOrPercentage = IOPInt Int
                     | IOPPercentage Double
  deriving (Show, Eq)

data IntOrRange = IORInt Int
                | IORRange Int Int
  deriving (Show, Eq)

instance ToJSON IntOrPercentage where
  toJSON (IOPInt n) = toJSON n
  toJSON (IOPPercentage p) = toJSON $ show p ++ "%"

instance FromJSON IntOrPercentage where
  parseJSON (Number n) = case toBoundedInteger n of
                           Just i -> return $ IOPInt i
                           Nothing -> fail $ "expected integer, but encountered " ++ show n
  parseJSON (String s) = IOPPercentage <$> parsePercentage s
  parseJSON x = typeMismatch "Int or Percentage" x

instance ToJSON IntOrRange where
  toJSON (IORInt n) = toJSON n
  toJSON (IORRange low high) = toJSON $ show low ++ "-" ++ show high

instance FromJSON IntOrRange where
  parseJSON (Number n) = case toBoundedInteger n of
                           Just i -> return $ IORInt i
                           Nothing -> fail $ "expected integer, but encountered" ++ show n
  parseJSON (String s) = do
    (low, high) <- parseRange s
    return $ IORRange low high

-- TODO: Fix this
parsePercentage :: Monad m => Text -> m Double
parsePercentage t = undefined

parseRange :: Monad m => Text -> m (Int, Int)
parseRange t = undefined

data InstanceGroupEnv = InstanceGroupEnv { igePersistentDiskFS :: Maybe Text
                                         , igePersistentDiskMountOptions :: Maybe [Text]
                                         , igeBOSH :: Maybe InstanceGroupBoshOpts
                                         }  deriving (Show, Eq, Generic)

data InstanceGroupBoshOpts = InstanceGroupBoshOpts { igboPassword :: Maybe Text
                                                   , igboKeepRootPassword :: Maybe Bool
                                                   , igbeRemoveDevTools :: Maybe Bool
                                                   , igbeRemoveStaticLibraries :: Maybe Bool
                                                   , igbeSwapSize :: Maybe Int
                                                   , igbeIPV6 :: Maybe Enableable
                                                   , igbeJobDir :: Maybe JobDir
                                                   , igbeAgent :: Maybe BoshAgentOpts}  deriving (Show, Eq, Generic)

data Enableable = Enableable { enable :: (Maybe Bool)}
  deriving (Show, Eq, Generic)

data JobDir = JobDir { jdTmpFS :: Maybe Bool, jdTmpFSSize :: Maybe Text}  deriving (Show, Eq, Generic)

data BoshAgentOpts = BoshAgentOpts { baoSettings :: Maybe Value
                                   , baoTmpFS    :: Maybe Bool
                                   }  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''Addon)
$(deriveJSON (aesonPrefix snakeCase) ''BoshAgentOpts)
$(deriveJSON (aesonPrefix snakeCase) ''ConsumeFrom)
$(deriveJSON (aesonPrefix snakeCase) ''Enableable)
$(deriveJSON (aesonPrefix snakeCase) ''Features)
$(deriveJSON (aesonPrefix snakeCase) ''InstanceGroup)
$(deriveJSON (aesonPrefix snakeCase) ''InstanceGroupBoshOpts)
$(deriveJSON (aesonPrefix snakeCase) ''InstanceGroupEnv)
$(deriveJSON (aesonPrefix snakeCase) ''Job)
$(deriveJSON (aesonPrefix snakeCase) ''JobDir)
$(deriveJSON defaultOptions{ fieldLabelModifier = snakeCase } ''Manifest)
$(deriveJSON (aesonPrefix snakeCase) ''Network)
$(deriveJSON (aesonPrefix snakeCase) ''ProvideAs)
$(deriveJSON (aesonPrefix snakeCase) ''Release)
$(deriveJSON (aesonPrefix snakeCase) ''ReleaseStemcell)
$(deriveJSON (aesonPrefix snakeCase) ''Stemcell)
$(deriveJSON (aesonPrefix snakeCase) ''Update)
$(deriveJSON (aesonPrefix snakeCase) ''VmResources)
$(deriveJSON (aesonPrefix snakeCase) ''Variable)
