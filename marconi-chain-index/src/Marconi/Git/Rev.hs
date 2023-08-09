{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Git.Rev (
  gitRev,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Foreign.C.String (CString)
import GHC.Foreign (peekCStringLen)
import Marconi.Git.RevFromGit (gitRevFromGit)
import System.IO (utf8)
import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall "&_marconi_git_rev" c_gitrev :: CString

gitRev :: Text
gitRev
  | T.null fromGit = zeroRev
  | gitRevEmbed /= zeroRev = gitRevEmbed
  | otherwise = fromGit
  where
    -- Git revision embedded after compilation using
    -- Data.FileEmbed.injectWith. If nothing has been injected,
    -- this will be filled with 0 characters.
    gitRevEmbed :: Text
    gitRevEmbed = T.pack $ drop 28 $ unsafeDupablePerformIO (peekCStringLen utf8 (c_gitrev, 68))

fromGit :: Text

-- Git revision found during compilation by running git. If
-- git could not be run, then this will be empty.
-- cross compiling to arm fails; due to a linker bug
#if defined(arm_HOST_ARCH)
fromGit = ""
#else
fromGit = T.strip (T.pack $(gitRevFromGit))
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
