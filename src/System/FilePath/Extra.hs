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
import Data.Function ((.), ($), on)
import Data.Functor (Functor(fmap))
import Data.Int (Int)
import Data.List (drop, length, map, take, takeWhile, zip)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (fst, uncurry)
import System.IO (FilePath)

import System.FilePath ((</>), joinPath, splitDirectories)


-- | Drop prefix of a file path.
--
-- >>> dropPrefix "/home/joe" "/home/joe/devel/haskell/mega-project"
-- Just "devel/haskell/mega-project"
-- >>> dropPrefix "joe" "joe/devel/haskell/mega-project"
-- Just "devel/haskell/mega-project"
-- >>> dropPrefix "/home/joe" "devel/haskell/mega-project"
-- Nothing
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

-- | Unsafe version of 'dropPrefix'. It throws error when path is not starting
-- with prefix.
unsafeDropPrefix :: FilePath -> FilePath -> FilePath
unsafeDropPrefix prefix path = case dropPrefix prefix path of
    Just fp -> fp
    Nothing -> error "unsafeDropPrefix: Path doesn't start with prefix."

-- | First drop \"old\" prefix and then insert new one.
--
-- @
-- changePrefixTo oldPrefix newPrefix =
--     'fmap' (newPrefix '</>') '.' 'dropPrefix' oldPrefix
-- @
changePrefixTo
    :: FilePath
    -- ^ Old prefix.
    -> FilePath
    -- ^ New prefix.
    -> FilePath
    -- ^ Path starting with old prefix.
    -> Maybe FilePath
changePrefixTo oldPrefix newPrefix =
    fmap (newPrefix </>) . dropPrefix oldPrefix

-- | Unsafe version of 'changePrefixTo'. It throws error when path is not
-- starting with prefix.
unsafeChangePrefixTo :: FilePath -> FilePath -> FilePath -> FilePath
unsafeChangePrefixTo oldPrefix newPrefix path =
    case changePrefixTo oldPrefix newPrefix path of
        Just fp -> fp
        Nothing -> error
            "unsafeChangePrefixTo: Path doesn't start with prefix."

-- | Check if path starts with prefix.
--
-- @
-- path `hasPrefix` prefix <==> 'isJust' ('dropPrefix' prefix path)
-- @
hasPrefix :: FilePath -> FilePath -> Bool
hasPrefix = (isJust .) . dropPrefix

-- | Drop specified number of directories from the start of a path.
--
-- Note that if path is absolute (starting with \"\/\" on UNIX\/Linux), then
-- it is considered as a first directory.
--
-- >>> dropDirectories 1 "/home/joe/devel/haskell/mega-project"
-- "home/joe/devel/haskell/mega-project"
-- >>> dropDirectories 2 "/home/joe/devel/haskell/mega-project"
-- "joe/devel/haskell/mega-project"
-- >>> dropDirectories 3 "/home/joe/devel/haskell/mega-project"
-- "devel/haskell/mega-project"
-- >>> dropDirectories 100 "/home/joe/devel/haskell/mega-project"
-- ""
-- >>> dropDirectories 0 "/home/joe/devel/haskell/mega-project"
-- "/home/joe/devel/haskell/mega-project"
-- >>> dropDirectories (-10) "/home/joe/devel/haskell/mega-project"
-- "/home/joe/devel/haskell/mega-project"
-- >>> dropDirectories 2 "devel/haskell/mega-project"
-- "mega-project"
dropDirectories :: Int -> FilePath -> FilePath
dropDirectories n = joinPath . drop n . splitDirectories

-- | Take specified number of directories from the start of a path.
--
-- >>> takeDirectories 1 "/home/joe/devel"
-- "/"
-- >>> takeDirectories 2 "/home/joe/devel"
-- "/home"
-- >>> takeDirectories 3 "/home/joe/devel"
-- "/home/joe"
-- >>> takeDirectories 10 "/home/joe/devel"
-- "/home/joe/devel"
-- >>> takeDirectories 0 "/home/joe/devel"
-- ""
-- >>> takeDirectories (-10) "/home/joe/devel"
-- ""
-- >>> takeDirectories 2 "usr/local/bin"
-- "usr/local"
takeDirectories :: Int -> FilePath -> FilePath
takeDirectories n = joinPath . take n . splitDirectories

-- | Find longest common prefix of two paths.
--
-- >>> commonPrefix "/home/joe/devel" "/home/joe/bin"
-- "/home/joe"
-- >>> commonPrefix "/home/joe/devel" "/home/jim/bin"
-- "/home"
-- >>> commonPrefix "/home/joe/devel" "/usr/bin"
-- "/"
-- >>> commonPrefix "/usr/local/bin" "usr/local/bin"
-- ""
commonPrefix :: FilePath -> FilePath -> FilePath
commonPrefix path1 path2 = joinPath
    . map fst . takeWhile (uncurry (==))
    $ (zip `on` splitDirectories) path1 path2

-- | Drop longest common prefix of two paths.
--
-- >>> dropCommonPrefix "/home/joe/devel" "/home/joe/bin"
-- ("devel", "bin")
-- >>> dropCommonPrefix "/home/joe" "/home/joe/bin"
-- ("", "bin")
-- >>> dropCommonPrefix "" "/home/joe/bin"
-- ("", "/home/joe/bin")
-- >>> dropCommonPrefix "/" "/home/joe/bin"
-- ("", "home/joe/bin")
dropCommonPrefix :: FilePath -> FilePath -> (FilePath, FilePath)
dropCommonPrefix path1 path2 =
    ((,) `on` joinPath . drop prefixLen) path1' path2'
  where
    (path1', path2') = ((,) `on` splitDirectories) path1 path2
    prefixLen = length . takeWhile (uncurry (==))
        $ (zip `on` splitDirectories) path1 path2
