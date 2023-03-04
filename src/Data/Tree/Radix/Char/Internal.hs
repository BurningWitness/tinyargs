module Data.Tree.Radix.Char.Internal
  ( Tree (..)
  , Edge (..)
  , empty
  , singleton
  , lookup
  , insert
  , insertWith
  , showTree
  ) where

import           Data.Char (chr, ord)
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Data.List hiding (insert, lookup, singleton)
import           Prelude hiding (lookup)



data Tree a = Empty
            | Tree (Edge a)

data Edge a =
       Edge
         String            -- ^ Label
         (Maybe a)         -- ^ Leaf, if exists
         (IntMap (Edge a)) -- ^ Edges. First character of each edge serves as a key
                           --   and is thus excluded from the edge's label.



-- | Empty tree.
empty :: Tree a
empty = Empty

-- | Tree with a single entry.
singleton :: String -> a -> Tree a
singleton as v = Tree $ Edge as (Just v) IntMap.empty



-- | Lookup the value at a key in the map.
--
--   The key is traversed lazily, left to right, comparing each character at most once.
lookup :: String -> Tree a -> Maybe a
lookup xs (Tree tree) = go xs tree
  where
    go as (Edge bs mayv t) = do
      cs <- stripPrefix bs as
      case cs of
        c:ct -> do
          o <- IntMap.lookup (ord c) t
          go ct o

        []   -> mayv

lookup _  Empty       = Nothing



-- | Common prefix of the two lists.
--
--   Return is @(prefix, remainder of first list, remainder of second list)@.
commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix as bs
  | a:at <- as, b:bt <- bs, a == b = let (xs, ys, zs) = commonPrefix at bt
                                     in (a:xs, ys, zs)

  | otherwise                      = ([], as, bs)



-- | Insert a new key and value into the tree. If the key is already present
--   in the tree, the associated value is replaced with the supplied one.
--
--   @'insert'@ is equivalent to @\\k v -> 'insertWith' ('const' v) k v@.
insert :: String -> a -> Tree a -> Tree a
insert ls v = insertWith (const v) ls v

-- | Insert a new key and value into the tree. If the key is already present
--   in the tree, the supplied function is applied to it instead.
--
--   The key is traversed lazily, left to right, comparing each character at most once.
insertWith :: (a -> a) -> String -> a -> Tree a -> Tree a
insertWith f ls v (Tree tree) = Tree $ go ls tree
  where
    go as (Edge bs mayx t) =
      let (common, xs, ys) = commonPrefix as bs
      in case (xs, ys) of
           (x:xt, _) ->
             case ys of
               y:yt -> Edge common Nothing .
                         IntMap.insert (ord y) (Edge yt mayx t) $
                           IntMap.singleton (ord x) (Edge xt (Just v) IntMap.empty)

               []   -> Edge common mayx .
                         (\g -> IntMap.alter g (ord x) t) $ \mayEdge ->
                            Just $ case mayEdge of
                                     Nothing -> Edge xt (Just v) IntMap.empty
                                     Just o  -> go xt o

           ([]  , _) ->
             case ys of
               y:yt -> Edge common (Just v) $
                         IntMap.singleton (ord y) (Edge yt mayx t)

               []   -> Edge common (Just $ maybe v f mayx) t

insertWith _ as v Empty = singleton as v



-- | Debug printing.
showTree :: Show a => Tree a -> String
showTree Empty                      = "<empty>"
showTree (Tree (Edge xs mayx deep)) =
  show xs <> foldMap (mappend " := " . show) mayx <> "\n" <> dive (mappend xs) deep
  where
    go prefix (Edge as mayv deeper) =
      show (prefix as) <> foldMap (mappend " := " . show) mayv <> "\n"
        <> dive (prefix . mappend as) deeper

    dive prefix =
      IntMap.foldrWithKey (\k v -> mappend $ go (mappend $ prefix [chr k]) v) []
