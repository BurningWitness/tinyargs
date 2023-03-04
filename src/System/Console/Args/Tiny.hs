{-# LANGUAGE RecordWildCards #-}

{- | Short flexible argument templater following
     [POSIX](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html)
     and [GNU](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
     conventions for providing commandline options.

     The templates only transform the bare minimum to fit the expectations. It's up to
     you to ensure (or disregard) the following rules:

     - Short 'Name's should be alphanumeric,
       long 'Name's should be alphanumeric and may contain dashes;

     - Option argument descriptions (in 'Flavor's) should be alphanumeric;

     - Option name list together with argument description (if any) and padding
       should take up less than 'formBlockWidth' characters;

     - Text provided in 'Block' and `optInfo' should be formatted in a sane fashion.

         Note that this library does not provide any data-level newline support,
         putting newlines (@'\\n'@) in text is the expected method of indentation.

     = Sidecases

     For the sidecases that follow let us assume that:

     - @-a@ and @-b@ are short options that take no arguments;

     - @-o@ is a short option that takes some argument
       (either flavor, unless sidecase states otherwise);

     - @--option@ is a long option that takes some argument
       (either flavor, unless sidecase states otherwise).

     Thus:

     - For short options correct usage is @-ofoo@ or @-o foo@.

         @-o=foo@ will be interpreted as @-o =foo@;

         @-o-a@ will be interpreted as @-o@ taking @-a@ as an argument;

         @-o--option@ will be interpreted as @-o@ taking @--option@ as an argument.

     - For applying optional arguments the only correct usage is @-ofoo@
       and @--option=foo@.

         @-o foo@ will be interpreted as @-o@ taking no argument and
         @foo@ as a non-option argument;

         @--option foo@ will be interpreted as @--option@ taking no argument and
         @foo@ as a non-option argument;

     - Options that require arguments consume said arguments indiscriminately.

         @-o -a -b@ will be interpreted as @-o-a -b@;

         @--option -a -b@ will be interpreted as @--option=-a -b@.

     - Multiple short arguments can be stacked together, but only the last one is allowed
       to take an argument.

         @-abofoo@ will be interpreted as @-a -b -ofoo@;

         @-aobfoo@ will be interpreted as @-a -obfoo@.
 -}

module System.Console.Args.Tiny
  ( -- * Types
    Name (..)
  , Flavor (..)
  , Option (..)
  , Block (..)
  , option
  , xoption
    -- * Composing help
  , Format (..)
  , defFormat
  , composeHelp
    -- * Parsing arguments
  , Order (..)
  , parseArgs
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Tree.Radix.Char as Radix

import           Data.Char (ord)
import qualified Data.List as List
import           Data.Maybe



-- | Option name.
data Name = Short Char  -- ^ As in @-s@.
          | Long String -- ^ As in @--long-option@.

-- | Option behavior.
data Flavor s = -- | Option takes no arguments.
                Plain (s -> Either String s)
                -- | Option takes an optional argument.
              | Optional
                  String -- ^ Help description for the argument
                  (s -> Maybe String -> Either String s)
                -- | Option requires an argument.
              | Required
                  String -- ^ Help description for the argument
                  (s -> String -> Either String s)

data Option s =
       Option
         { optHidden :: Bool     -- ^ If 'True', this option is hidden from help.
         , optNames  :: [Name]   -- ^ If empty, this option is both hidden from help
                                 --   and impossible to invoke.
         , optInfo   :: String   -- ^ Help description, shown to the right
                                 --   (and below if needed) of the option name list.
         , optFlavor :: Flavor s
         }



data Block s = Block String
             | Opt (Option s)

-- | Shorthand for a visible 'Option'.
option :: [Name] -> String -> Flavor s -> Block s
option name info = Opt . Option False name info

-- | Shorthand for a hidden 'Option'.
xoption :: [Name] -> String -> Flavor s -> Block s
xoption name info = Opt . Option True name info



data Format =
       Format
         { formShortOffset :: Int -- ^ Offset of an option that starts with
                                  --   a short argument.
         , formLongOffset  :: Int -- ^ Offset of an option that starts with
                                  --   a long argument.
         , formDescPadding :: Int -- ^ If flags take up more than 'formDescOffset',
                                  --   but description can still be fit in the remaining
                                  --   space, this is the padding increment its aligned
                                  --   against.
         , formDescOffset  :: Int -- ^ Option description offset.
         , formLineWidth   :: Int -- ^ Total width of a line.
         }

-- | @
--'formShortOffset' = 2
--'formLongOffset'  = 6
--'formDescPadding' = 5
--'formDescOffset'  = 30
--'formLineWidth'   = 80 @
defFormat :: Format
defFormat =
  Format
    { formShortOffset = 2
    , formLongOffset  = 6
    , formDescPadding = 5
    , formDescOffset  = 30
    , formLineWidth   = 80
    }



composeName :: Name -> String
composeName (Short s) = '-' : s : []
composeName (Long l)  = '-' : '-' : l

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn _ [] = []
breakOn f as = let (bef, aft) = break f as
               in bef : case aft of
                          []   -> []
                          _:[] -> [[]]
                          _:bs -> breakOn f bs

splitInfo :: Int -> String -> [String]
splitInfo n = foldMap subdivide . breakOn (== '\n')
  where
    subdivide s =
      let (xs, ys) = span (< n) $ List.elemIndices ' ' s
      in case (xs, ys) of
           ([], _)  -> [s]
           (_ , []) -> [s]
           _        -> let (bef, aft) = List.splitAt (List.last xs) s
                       in bef : subdivide (List.drop 1 aft)

remup :: Int -> Int -> Int
remup a n = a + n - rem a n

pad :: Char -> Int -> String -> String
pad c n s = s <> replicate (n - length s) c

composeOption :: Format -> Option s -> String
composeOption Format {..} Option {..} =
  let flags = case optNames of
                Short _ : _ -> replicate formShortOffset ' '
                _           -> replicate formLongOffset ' '
           <> List.intercalate ", " (composeName <$> optNames)
           <> case optFlavor of
                Plain         _ -> []
                Optional name _ -> case List.last optNames of
                                     Short _ -> '[' :       name <> "]"
                                     Long _  -> '[' : '=' : name <> "]"
                Required name _ -> case List.last optNames of
                                     Short _ -> ' ' : name
                                     Long _  -> '=' : name

  in case () of
       () | length flags < formDescOffset ->
              pad ' ' formDescOffset flags
           <> List.intercalate ('\n' : replicate formDescOffset ' ')
                               (splitInfo (formLineWidth - formDescOffset) optInfo)

          | single:[] <- splitInfo (formLineWidth - formDescOffset) optInfo
          , (length flags `remup` formDescPadding) + length single < formLineWidth ->
              pad ' ' (length flags `remup` formDescPadding) flags
           <> single

          | otherwise ->
              flags
           <> foldMap (\s -> '\n' : replicate formDescOffset ' ' <> s)
                      (splitInfo (formLineWidth - formDescOffset) optInfo)

composeLine :: Format -> String -> String
composeLine format = List.intercalate "\n" . splitInfo (formLineWidth format)

-- | Composes a string in the format typically used for @--help@ options.
composeHelp :: Format -> [Block s] -> String
composeHelp format = foldr combine []
  where
    combine (Block text) = mappend (composeLine format text) . (:) '\n'
    combine (Opt opt) | optHidden opt = id
                      | otherwise     = mappend (composeOption format opt) . (:) '\n'



-- | Similar to "System.Console.GetOpt.ArgOrder".
data Order s = RequireOrder -- ^ First non-option marks end of options
             | Permute      -- ^ Non-options are freely interspersed with options
             | ReturnInOrder (s -> String -> Either String s)
               -- ^ Parse every non-option argument with the provided function

data State s = Full
             | Give (s -> Maybe String -> Either String s)
             | Take Name (s -> String -> Either String s)



argmaps :: [Block s] -> (IntMap (Flavor s), Radix.Tree (Flavor s))
argmaps = foldr go (IntMap.empty, Radix.empty) . mapMaybe wither
  where
    wither (Opt opt) = Just opt
    wither _         = Nothing

    go o acc = foldr (subgo $ optFlavor o) acc (optNames o)

    subgo f (Short s) (ss, ls) = (IntMap.insert (ord s) f ss, ls)
    subgo f (Long l)  (ss, ls) = (ss, Radix.insert l f ls)



-- | Parses the provided list of commandline arguments.
--
--   The argument list is traversed left to right.
--
--   The return is either an error or an altered state together with
--   a list of non-option arguments.
parseArgs :: Order s -> [Block s] -> s -> [String] -> Either String (s, [String])
parseArgs order0 parts s0 args0 = go order0 Full [] args0 s0
  where
    (shortmap, longmap) = argmaps parts

    goshort order reqs args []           s = go order Full reqs args s
    goshort order reqs args (short:rest) s =
      case IntMap.lookup (ord short) shortmap of
        Nothing     -> Left $ "Unrecognized argument " <> composeName (Short short)
        Just flavor ->
          case flavor of
            Plain f ->
              f s >>= case rest of
                        [] -> go      order Full reqs args
                        _  -> goshort order      reqs args rest

            Optional _ f ->
               let input = case rest of
                             [] -> Nothing
                             _  -> Just rest

               in go order Full reqs args =<< f s input

            Required _ f ->
              case rest of
                [] -> go order (Take (Short short) f) reqs args s
                _  -> go order Full reqs args =<< f s rest

    go _     state reqs      []    s =
      case state of
        Full         -> Right (s, reverse reqs)
        Give f       -> flip (,) (reverse reqs) <$> f s Nothing
        Take name _f -> Left $ "Option " <> composeName name <> " requires an argument"

    go order state reqs (arg:args) s =
      case state of
        Full ->
          case arg of
            '-':'-':[]       -> Right (s, reverse reqs <> args)

            '-':'-':verylong ->
              let (long, rawvalue) = List.span (/= '=') verylong
              in case Radix.lookup long longmap of
                   Nothing     -> Left $ "Unrecognized option " <> composeName (Long long)
                   Just flavor ->
                     case flavor of
                       Plain f ->
                         case rawvalue of
                           [] -> go order Full reqs args =<< f s
                           _  -> Left $ "Option " <> composeName (Long long) <> " does not accept arguments"

                       Optional _ f ->
                         let input = case rawvalue of
                                       [] -> Nothing
                                       _  -> Just $ drop 1 rawvalue

                         in go order Full reqs args =<< f s input

                       Required _ f ->
                         case rawvalue of
                           [] -> go order (Take (Long long) f) reqs args s
                           _  -> go order Full reqs args =<< f s (drop 1 rawvalue)

            '-':rest -> goshort order reqs args rest s

            _            ->
              case order of
                RequireOrder    -> Right (s, reverse reqs <> (arg:args))
                Permute         -> go order Full (arg:reqs) args s
                ReturnInOrder f -> go order Full reqs args =<< f s arg

        Give f ->
          case arg of
            '-':_ -> go order Full reqs (arg:args) =<< f s Nothing
            _     -> go order Full reqs      args  =<< f s (Just arg)

        Take _name f -> go order Full reqs args =<< f s arg
