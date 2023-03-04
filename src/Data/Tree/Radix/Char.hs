{-# LANGUAGE NoImplicitPrelude #-}

{- | Simple radix tree over lists of characters.

     Only 'lookup' and 'insert' are implemented because that's all an argument
     parser needs.
 -}

module Data.Tree.Radix.Char
  ( Tree
  , empty
  , singleton
  , lookup
  , insert
  , insertWith
  ) where

import           Data.Tree.Radix.Char.Internal
