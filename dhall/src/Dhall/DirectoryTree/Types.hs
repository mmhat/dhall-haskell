{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types used by the implementation of the @to-directory-tree@ subcommand
module Dhall.DirectoryTree.Types
    ( DirectoryTree(..)
    , FilesystemEntry(..)
    , DirectoryEntry
    , FileEntry
    , Entry(..)
    , User(..)
    , Group(..)
    , Mode(..)
    , Access(..)

    , setFileMode
    , prettyFileMode

    , isMetadataSupported
    ) where

import Data.Coerce                (coerce)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity      (Identity (..))
import Data.Sequence              (Seq)
import Data.Text                  (Text)
import Data.Void                  (Void)
import Data.Word                  (Word32)
import Dhall.Marshal.Internal     (Generic, InputNormalizer, InterpretOptions (..), defaultInputNormalizer, defaultInterpretOptions)
import Dhall.Marshal.Decode       ( Decoder (..), FromDhall (..), Expector)
import Dhall.Marshal.Encode       (Encoder(..), ToDhall(..))
import Dhall.Src                  (Src)
import Dhall.Syntax               (Const(..), Expr (..), FieldSelection (..), RecordField, Var (..), makeFieldSelection, makeFunctionBinding, makeRecordField)
import System.PosixCompat.Types   (GroupID, UserID)
import Dhall.Core                 (alphaNormalize, denote)

import qualified Data.Sequence            as Seq
import qualified Data.Text                as Text
import qualified Dhall.Map                as Map
import qualified Dhall.Marshal.Decode     as Decode
import qualified Dhall.Marshal.Encode     as Encode
import qualified System.PosixCompat.Files as Posix

#ifdef mingw32_HOST_OS
import Control.Monad            (unless)
import System.IO                (hPutStrLn, stderr)
import System.PosixCompat.Types (CMode)

import qualified Unsafe.Coerce

type FileMode = CMode
#else
import System.PosixCompat.Types (FileMode)

import qualified System.PosixCompat.Types as Posix
#endif

pattern Make :: Text -> Expr s a -> Expr s a
pattern Make label entry <- App (Field (Var (V "_" 0)) (fieldSelectionLabel -> label)) entry

newtype DirectoryTree = DirectoryTree {unDirectoryTree :: Seq FilesystemEntry}
    deriving stock (Show)
    deriving newtype (Monoid, Semigroup)

instance FromDhall DirectoryTree where
    autoWith normalizer = Decoder
        { expected = directoryTreeExpector normalizer
        , extract = \expr -> case alphaNormalize . denote $ expr of
            Lam _ _ (Lam _ _ body) -> DirectoryTree <$> extract bodyDecoder body
            _ -> Decode.typeError (expected bodyDecoder) expr
        }
        where
            bodyDecoder :: Decoder (Seq FilesystemEntry)
            bodyDecoder = Decode.sequence (filesystemEntryDecoder normalizer)

instance ToDhall DirectoryTree where
    injectWith normalizer =
        Encoder
            { declared = directoryTreeType
            , embed = \(DirectoryTree entries) ->
                Lam Nothing (makeFunctionBinding "tree" (Const Type)) $
                    Lam Nothing (makeFunctionBinding "make" (App makeType (Var "tree"))) $
                        let
                            entries' = foldMap (Seq.singleton . embedEntry normalizer) entries
                        in
                            -- See https://github.com/dhall-lang/dhall-haskell/issues/1359
                            ( if Seq.null entries'
                                then ListLit (Just (App List (Var "tree")))
                                else ListLit Nothing
                            )
                                entries'
            }

newtype Tree = Tree {unTree :: Expr Src Void}

instance ToDhall Tree where
    injectWith _normalizer =
        Encoder
            { declared = Var "tree"
            , embed = unTree
            }

treeDecoder :: (Expr Src Void -> Decode.Extractor Src Void a) -> Decoder a
treeDecoder f =
        Decoder
            { expected = pure (Var "tree")
            , extract = f
            }

-- | The type of a fixpoint directory tree expression.
directoryTreeType :: Expr Src Void
directoryTreeType = Pi Nothing "tree" (Const Type)
    (Pi Nothing "make" makeType (App List (Var "tree")))

-- | The type of make part of a fixpoint directory tree expression.
makeType :: Expr Src Void
makeType = Record . Map.fromList $
    [ ("directory", makeRecordField (directoryEntryType defaultInputNormalizer))
    , ("file", makeRecordField (fileEntryType defaultInputNormalizer))
    ]

-- | The type of a fixpoint directory tree expression.
directoryTreeExpector :: InputNormalizer -> Expector (Expr Src Void)
directoryTreeExpector normalizer = Pi Nothing "tree" (Const Type)
    <$> ( Pi Nothing "make"
        <$> makeExpector normalizer
        <*> pure (App List (Var "tree"))
        )

-- | The type of make part of a fixpoint directory tree expression.
makeExpector :: InputNormalizer -> Expector (Expr Src Void)
makeExpector normalizer = Record . Map.fromList <$> sequenceA
    [ makeConstructor "directory" directoryEntryDecoder
    , makeConstructor "file" fileEntryDecoder
    ]
    where
        makeConstructor :: Text -> (InputNormalizer -> Decoder b) -> Expector (Text, RecordField Src Void)
        makeConstructor name makeDecoder = (name,) . makeRecordField
            <$> ( Pi Nothing "_"
                <$> expected (makeDecoder normalizer)
                <*> pure (Var "tree")
                )

-- | A directory in the filesystem.
type DirectoryEntry = Entry (Seq FilesystemEntry)

-- | A file in the filesystem.
type FileEntry = Entry Text

-- | A filesystem entry.
data FilesystemEntry
    = DirectoryEntry (Entry (Seq FilesystemEntry))
    | FileEntry (Entry Text)
    deriving (Eq, Generic, Ord, Show)

filesystemEntryDecoder :: InputNormalizer -> Decoder FilesystemEntry
filesystemEntryDecoder normalizer = treeDecoder $ \case
    Make "directory" entry ->
        DirectoryEntry <$> extract (directoryEntryDecoder normalizer) entry
    Make "file" entry ->
        FileEntry <$> extract (fileEntryDecoder normalizer) entry
    expr -> Decode.typeError (expected (filesystemEntryDecoder normalizer)) expr

embedEntry :: InputNormalizer -> FilesystemEntry -> Expr Src Void
embedEntry normalizer (DirectoryEntry directory) =
    embedDirectoryEntry normalizer directory
embedEntry normalizer (FileEntry file) =
    embedFileEntry normalizer file

-- | A generic filesystem entry. This type holds the metadata that apply to all
-- entries. It is parametric over the content of such an entry.
data Entry a = Entry
    { entryName :: String
    , entryContent :: a
    , entryUser :: Maybe User
    , entryGroup :: Maybe Group
    , entryMode :: Maybe (Mode Maybe)
    }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance FromDhall a => FromDhall (Entry a) where
    autoWith = Decode.genericAutoWithInputNormalizer entryInterpretOptions

directoryEntryDecoder :: InputNormalizer -> Decoder DirectoryEntry
directoryEntryDecoder = entryDecoder (Decode.sequence . filesystemEntryDecoder)

fileEntryDecoder :: InputNormalizer -> Decoder FileEntry
fileEntryDecoder = entryDecoder (const Decode.strictText)

entryDecoder :: (InputNormalizer -> Decoder a) -> InputNormalizer -> Decoder (Entry a)
entryDecoder nested normalizer = Decode.record
    ( Entry
        <$> Decode.field "name" (Decode.autoWith normalizer)
        <*> Decode.field "content" (nested normalizer)
        <*> Decode.field "user" (Decode.autoWith normalizer)
        <*> Decode.field "group" (Decode.autoWith normalizer)
        <*> Decode.field "mode" (Decode.autoWith normalizer)
    )

directoryEntryType :: InputNormalizer -> Expr Src Void
directoryEntryType normalizer =
    declared
      ( Encode.genericToDhallWithInputNormalizer
            @(Entry (Seq Tree))
            entryInterpretOptions
            normalizer
      )

embedDirectoryEntry :: InputNormalizer -> DirectoryEntry -> Expr Src Void
embedDirectoryEntry normalizer directory =
    App
        (Field (Var "make") (makeFieldSelection "directory"))
        ( embed
            ( Encode.genericToDhallWithInputNormalizer
                entryInterpretOptions
                normalizer
            )
            (fmap (fmap (Tree . embedEntry normalizer)) directory)
        )

fileEntryType :: InputNormalizer -> Expr Src Void
fileEntryType normalizer =
    declared
      ( Encode.genericToDhallWithInputNormalizer
            @FileEntry
            entryInterpretOptions
            normalizer
      )

embedFileEntry :: InputNormalizer -> FileEntry -> Expr Src Void
embedFileEntry normalizer file =
    App
        (Field (Var "make") (makeFieldSelection "file"))
        ( embed
            ( Encode.genericToDhallWithInputNormalizer
                entryInterpretOptions
                normalizer
            )
            file
        )

entryInterpretOptions :: InterpretOptions
entryInterpretOptions = defaultInterpretOptions
        { fieldModifier = Text.toLower . Text.drop (Text.length "entry")
        }

-- | A user identified either by id or name.
data User
    = UserId UserID
    | UserName String
    deriving (Eq, Generic, Ord, Show)

instance FromDhall User
instance ToDhall User

#ifdef mingw32_HOST_OS
instance FromDhall UserID where
    autoWith normalizer = Unsafe.Coerce.unsafeCoerce <$> autoWith @Word32 normalizer

instance ToDhall UserID where
    injectWith normalizer = Unsafe.Coerce.unsafeCoerce >$< injectWith @Word32 normalizer
#else
instance FromDhall Posix.CUid where
    autoWith normalizer = Posix.CUid <$> autoWith normalizer

instance ToDhall Posix.CUid where
    injectWith normalizer = coerce @_ @Word32 >$< injectWith normalizer
#endif

-- | A group identified either by id or name.
data Group
    = GroupId GroupID
    | GroupName String
    deriving (Eq, Generic, Ord, Show)

instance FromDhall Group
instance ToDhall Group

#ifdef mingw32_HOST_OS
instance FromDhall GroupID where
    autoWith normalizer = Unsafe.Coerce.unsafeCoerce <$> autoWith @Word32 normalizer

instance ToDhall GroupID where
    injectWith normalizer = Unsafe.Coerce.unsafeCoerce >$< injectWith @Word32 normalizer
#else
instance FromDhall Posix.CGid where
    autoWith normalizer = Posix.CGid <$> autoWith normalizer

instance ToDhall Posix.CGid where
    injectWith normalizer = coerce @_ @Word32 >$< injectWith normalizer
#endif

-- | A filesystem mode. See chmod(1).
-- The parameter is meant to be instantiated by either `Identity` or `Maybe`
-- depending on the completeness of the information:
--  * For data read from the filesystem it will be `Identity`.
--  * For user-supplied data it will be `Maybe` as we want to be able to set
--    only specific bits.
data Mode f = Mode
    { modeUser :: f (Access f)
    , modeGroup :: f (Access f)
    , modeOther :: f (Access f)
    }
    deriving Generic

deriving instance Eq (Mode Identity)
deriving instance Eq (Mode Maybe)
deriving instance Ord (Mode Identity)
deriving instance Ord (Mode Maybe)
deriving instance Show (Mode Identity)
deriving instance Show (Mode Maybe)

instance FromDhall (Mode Identity) where
    autoWith = modeDecoder

instance FromDhall (Mode Maybe) where
    autoWith = modeDecoder

modeDecoder :: FromDhall (f (Access f)) => InputNormalizer -> Decoder (Mode f)
modeDecoder = Decode.genericAutoWithInputNormalizer modeInterpretOptions

instance ToDhall (Mode Identity) where
    injectWith = modeEncoder

instance ToDhall (Mode Maybe) where
    injectWith = modeEncoder

modeEncoder :: ToDhall (f (Access f)) => InputNormalizer -> Encoder (Mode f)
modeEncoder = Encode.genericToDhallWithInputNormalizer modeInterpretOptions

modeInterpretOptions :: InterpretOptions
modeInterpretOptions = defaultInterpretOptions
    { fieldModifier = Text.toLower . Text.drop (Text.length "mode")
    }

-- | The permissions for a subject (user/group/other).
data Access f = Access
    { accessExecute :: f Bool
    , accessRead :: f Bool
    , accessWrite :: f Bool
    }
    deriving Generic

deriving instance Eq (Access Identity)
deriving instance Eq (Access Maybe)
deriving instance Ord (Access Identity)
deriving instance Ord (Access Maybe)
deriving instance Show (Access Identity)
deriving instance Show (Access Maybe)

instance FromDhall (Access Identity) where
    autoWith = accessDecoder

instance FromDhall (Access Maybe) where
    autoWith = accessDecoder

accessDecoder :: FromDhall (f Bool) => InputNormalizer -> Decoder (Access f)
accessDecoder = Decode.genericAutoWithInputNormalizer accessInterpretOptions

instance ToDhall (Access Identity) where
    injectWith = accessEncoder

instance ToDhall (Access Maybe) where
    injectWith = accessEncoder

accessEncoder :: ToDhall (f Bool) => InputNormalizer -> Encoder (Access f)
accessEncoder = Encode.genericToDhallWithInputNormalizer accessInterpretOptions

accessInterpretOptions :: InterpretOptions
accessInterpretOptions = defaultInterpretOptions
    { fieldModifier = Text.toLower . Text.drop (Text.length "access")
    }



-- | A wrapper around `Posix.setFileMode`. On Windows, it does check the
-- resulting file mode of the file/directory and emits a warning if it doesn't
-- match the desired file mode. On all other OS it is identical to
-- `Posix.setFileMode` as it is assumed to work correctly.
setFileMode :: FilePath -> FileMode -> IO ()
#ifdef mingw32_HOST_OS
setFileMode fp mode = do
    Posix.setFileMode fp mode
    mode' <- Posix.fileMode <$> Posix.getFileStatus fp
    unless (mode' == mode) $ hPutStrLn stderr $
        "Warning: Setting file mode did not succeed for " <> fp <> "\n" <>
        "    Expected: " <> prettyFileMode mode <> "\n" <>
        "    Actual:   " <> prettyFileMode mode'
#else
setFileMode fp mode = Posix.setFileMode fp mode
#endif

-- | Pretty-print a `FileMode`. The format is similar to the one ls(1):
-- It is display as three blocks of three characters. The first block are the
-- permissions of the user, the second one are the ones of the group and the
-- third one the ones of other subjects. A @r@ denotes that the file or
-- directory is readable by the subject, a @w@ denotes that it is writable and
-- an @x@ denotes that it is executable. Unset permissions are represented by
-- @-@.
prettyFileMode :: FileMode -> String
prettyFileMode mode = userPP <> groupPP <> otherPP
    where
        userPP :: String
        userPP =
            isBitSet 'r' Posix.ownerReadMode <>
            isBitSet 'w' Posix.ownerWriteMode <>
            isBitSet 'x' Posix.ownerExecuteMode

        groupPP :: String
        groupPP =
            isBitSet 'r' Posix.groupReadMode <>
            isBitSet 'w' Posix.groupWriteMode <>
            isBitSet 'x' Posix.groupExecuteMode

        otherPP :: String
        otherPP =
            isBitSet 'r' Posix.otherReadMode <>
            isBitSet 'w' Posix.otherWriteMode <>
            isBitSet 'x' Posix.otherExecuteMode

        isBitSet :: Char -> FileMode -> String
        isBitSet c mask = if mask `Posix.intersectFileModes` mode /= Posix.nullFileMode
            then [c]
            else "-"

-- | Is setting metadata supported on this platform or not.
isMetadataSupported :: Bool
#ifdef mingw32_HOST_OS
isMetadataSupported = False
#else
isMetadataSupported = True
#endif
