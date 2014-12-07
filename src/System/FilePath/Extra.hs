{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Functions missing in filepath library
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Functions missing in filepath library.
module System.FilePath.Extra
    ( dropPrefix
    , unsafeDropPrefix
    , changePrefixTo
    , unsafeChangePrefixTo
    , hasPrefix
    , dropDirectories
    , takeDirectories
    )
  where

import Prelude (error)

import Data.Bool (Bool, otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap))
import Data.Int (Int)
import Data.List (drop, take)
import Data.Maybe (Maybe(..), isJust)
import System.IO (FilePath)

import System.FilePath ((</>), joinPath, splitDirectories)


dropPrefix :: FilePath -> FilePath -> Maybe FilePath
dropPrefix prefix path = dropPrefix' prefix' path'
  where
    prefix' = splitDirectories prefix
    path' = splitDirectories path

    dropPrefix' :: [FilePath] -> [FilePath] -> Maybe FilePath
    dropPrefix' []       ys       = Just $ joinPath ys
    dropPrefix' (x : xs) (y : ys)
      | x == y    = dropPrefix' xs ys
      | otherwise = Nothing

unsafeDropPrefix :: FilePath -> FilePath -> FilePath
unsafeDropPrefix prefix path = case dropPrefix prefix path of
    Just fp -> fp
    Nothing -> error "unsafeDropPrefix: Path doesn't start with prefix."

changePrefixTo :: FilePath -> FilePath -> FilePath -> Maybe FilePath
changePrefixTo oldPrefix newPrefix = fmap (newPrefix </>) . dropPrefix oldPrefix

unsafeChangePrefixTo :: FilePath -> FilePath -> FilePath -> FilePath
unsafeChangePrefixTo oldPrefix newPrefix path =
    case changePrefixTo oldPrefix newPrefix path of
        Just fp -> fp
        Nothing -> error
            "unsafeChangePrefixTo: Path doesn't start with prefix."

hasPrefix :: FilePath -> FilePath -> Bool
hasPrefix = (isJust .) . dropPrefix

dropDirectories :: Int -> FilePath -> FilePath
dropDirectories n = joinPath . drop n . splitDirectories

takeDirectories :: Int -> FilePath -> FilePath
takeDirectories n = joinPath . take n . splitDirectories
