{- | Simple radix tree over lists.

     Specialized over 'Char' and thus uses 'IntMap's instead of 'Data.Map.Map's internally
     (hence the integer conversion functions passed to functions).

     Only lookups and insertions are implemented because that's all an argument
     parser needs.
 -}

module Data.RadixTree.Internal
  ( RadixTree (..)
  , Edge (..)
  , empty
  , singleton
  , lookup
  , insert
  , showTree
  ) where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Data.List hiding (insert, lookup, singleton)
import           Prelude hiding (lookup)



data RadixTree k a = Empty
                   | Tree (Edge k a)

data Edge k a =
       Edge
         [k]                 -- ^ Label
         (Maybe a)           -- ^ Leaf, if exists
         (IntMap (Edge k a)) -- ^ Edges. First character of each edge serves as a key
                             --   and is thus excluded from the edge's label.



empty :: RadixTree k a
empty = Empty

singleton :: [k] -> a -> RadixTree k a
singleton as v = Tree $ Edge as (Just v) IntMap.empty



{-# SPECIALISE lookup :: (Char -> Int) -> String -> RadixTree Char a -> Maybe a #-}
lookup :: Eq k => (k -> Int) -> [k] -> RadixTree k a -> Maybe a
lookup conv xs (Tree tree) = go xs tree
  where
    go as (Edge bs mayv t) = do
      cs <- stripPrefix bs as
      case cs of
        c:ct -> do
          o <- IntMap.lookup (conv c) t
          go ct o

        []   -> mayv

lookup _    _  Empty       = Nothing



-- | Common prefix of the two lists.
--
--   Return is (prefix, remainder of first list, remainder of second list).
commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix as bs
  | a:at <- as, b:bt <- bs, a == b = let (xs, ys, zs) = commonPrefix at bt
                                     in (a:xs, ys, zs)

  | otherwise                      = ([], as, bs)



{-# SPECIALISE insert :: (Char -> Int) -> String -> a -> RadixTree Char a -> RadixTree Char a #-}
insert :: Eq k => (k -> Int) -> [k] -> a -> RadixTree k a -> RadixTree k a
insert conv ls v (Tree tree) = Tree $ go ls tree
  where
    go as (Edge bs mayx t) =
      let (common, xs, ys) = commonPrefix as bs
      in case (xs, ys) of
           (x:xt, _) ->
             case ys of
               y:yt -> Edge common Nothing .
                         IntMap.insert (conv y) (Edge yt mayx t) $
                           IntMap.singleton (conv x) (Edge xt (Just v) IntMap.empty)

               [] -> Edge common mayx .
                       (\f -> IntMap.alter f (conv x) t) $ \mayEdge ->
                          Just $ case mayEdge of
                                   Nothing -> Edge xt (Just v) IntMap.empty
                                   Just o  -> go xt o

           ([]  , _) -> Edge common (Just v) t

insert _    as v Empty = singleton as v



-- | Debug printing.
showTree :: (Show k, Show a) => (Int -> k) -> RadixTree k a -> String
showTree _    Empty                      = "<empty>"
showTree conv (Tree (Edge xs mayx deep)) =
  show xs <> foldMap (mappend " := " . show) mayx <> "\n" <> dive (mappend xs) deep
  where
    go prefix (Edge as mayv deeper) =
      show (prefix as) <> foldMap (mappend " := " . show) mayv <> "\n"
        <> dive (prefix . mappend as) deeper

    dive prefix =
      IntMap.foldrWithKey (\k v -> mappend $ go (mappend $ prefix [conv k]) v) []
