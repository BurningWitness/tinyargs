{-# LANGUAGE FlexibleInstances
           , Rank2Types
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Tree.Radix.Char as Radix
import           System.Console.Args.Tiny

import           Data.Either
import           Data.Maybe
import           Data.Functor.Identity
import           Prelude hiding (lookup)
import           Test.Hspec hiding (Example, example)



deriving instance Show Name

deriving instance Show (Reason String)
deriving instance Show (Failure String)

deriving instance Eq Name
deriving instance Eq e => Eq (Reason e)
deriving instance Eq e => Eq (Failure e)

data Example =
       Example
         { _one   :: Maybe String
         , _two   :: Maybe String
         , _three :: Maybe String
         , _four  :: Maybe String
         , _five  :: Maybe String
         , _six   :: Maybe String
         , _seven :: Maybe String
         }
       deriving (Show, Eq)

defExample :: Example
defExample =
  Example
    { _one   = Nothing
    , _two   = Nothing
    , _three = Nothing
    , _four  = Nothing
    , _five  = Nothing
    , _six   = Nothing
    , _seven = Nothing
    }

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

set :: Lens' s a -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

one, two, three, four, five, six, seven :: Lens' Example (Maybe String)
one   f e = (\x -> e { _one   = x }) <$> f (_one e)
two   f e = (\x -> e { _two   = x }) <$> f (_two e)
three f e = (\x -> e { _three = x }) <$> f (_three e)
four  f e = (\x -> e { _four  = x }) <$> f (_four e)
five  f e = (\x -> e { _five  = x }) <$> f (_five e)
six   f e = (\x -> e { _six   = x }) <$> f (_six e)
seven f e = (\x -> e { _seven = x }) <$> f (_seven e)

flag :: Lens' s a -> (a -> Either e a) -> Flavor e s
flag lens f = Plain $ lens f

opt :: String -> Lens' s a -> (a -> Maybe String -> Either e a) -> Flavor e s
opt name lens f = Optional name $ \s o -> lens (flip f o) s

req :: String -> Lens' s a -> (a -> String -> Either e a) -> Flavor e s
req name lens f = Required name $ \s o -> lens (flip f o) s



example :: [Block String Example]
example =
  [ Block $
      mconcat
        [ "Usage: example [OPTION] [FILE]\n"
        , "\n"
        , "Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
        , "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
        , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
        , "nisi ut aliquip ex ea commodo consequat.\n"
        ]
  , option [Short '1', Long "option-one"] "First option" $
      flag one $ \_ -> Right (Just "flag")
  , let longdescr = mconcat
                      [ "Second option. Lorem ipsum dolor sit amet, consectetur "
                      , "adipiscing elit, sed do eiusmod tempor incididunt ut "
                      , "labore et dolore magna aliqua."
                      ]
    in option [Short '2', Long "option-two"] longdescr $
         opt "ARG2" two $ \old m -> Right $ Just (fromMaybe "empty" m) <> old

  , xoption [Long "option-three", Short '3'] "Third option" $
      req "ARG3" three $ \old i -> Right $ Just i <> old
  , Block "\nSuspiciously empty category"
  , xoption [Short '4', Long "option-four"] "Sneaky fourth option" $
      req "ARG4" four $ \_ -> Right . Just
  , Block "\nSmaller category"
  , option [Short '5', Long "option-five"] "Fifth option\nAlso a newline" $
      req "ARG5" five $ \_ -> Left . (<>) "No "
  , Block "\nLarger category"
  , option [Long "option-six", Long "synonym-for-option-six", Short '6']
      "Indented sixth option"
      . opt "ARG6" six $ \_ m -> Left $ case m of
                                          Just msg -> "No " <> msg
                                          Nothing  -> "Empty"
  , option [ Short '7', Long "option-seven", Long "synonym-for-option-seven"
           , Long "another-option-seven" ] "Overbearing seventh option" $
      flag seven $ \_ -> Right (Just "other flag")
  ]



run :: Order String Example -> [String] -> Either (Failure String) (Example, [String])
run order = parseArgs order example defExample

main :: IO ()
main =
  hspec $ do
    describe "Radix tree" $ do
      describe "Top level lookups" $ do
        it "Singleton" $ do
          Radix.lookup "this" (Radix.singleton "this" 1) `shouldBe` Just (1 :: Int)

        it "Unrelated" $ do
          Radix.lookup "this"
            (Radix.insert "separate" 2 $ Radix.singleton "this" 1)
            `shouldBe` Just (1 :: Int)

        it "Single split" $ do
          Radix.lookup "this" (Radix.insert "that" 2 $ Radix.singleton "this" 1)
            `shouldBe` Just (1 :: Int)

        it "Double split" $ do
          Radix.lookup "this"
            ( Radix.insert "third" 3
               . Radix.insert "that" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (1 :: Int)

        it "Two-level split" $ do
          Radix.lookup "this"
            ( Radix.insert "trench" 3
               . Radix.insert "tat" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (1 :: Int)

        it "Extending" $ do
          Radix.lookup "thistle"
            ( Radix.insert "thistle" 2 $ Radix.singleton "this" 1 )
            `shouldBe` Just (2 :: Int)

        it "Undercutting" $ do
          Radix.lookup "this"
            ( Radix.insert "this" 2 $ Radix.singleton "thistle" 1 )
            `shouldBe` Just (2 :: Int)

        it "Clobber" $ do
          Radix.lookup "this"
            ( Radix.insertWith (+2) "this" 2 $ Radix.singleton "this" 1 )
            `shouldBe` Just (3 :: Int)

      describe "Second level lookups" $ do
        it "Single" $ do
          Radix.lookup "thief"
            (Radix.insert "thief" 2 $ Radix.singleton "this" 1)
            `shouldBe` Just (2 :: Int)

        it "Unrelated" $ do
          Radix.lookup "thief"
            ( Radix.insert "separate" 3
                . Radix.insert "thief" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (2 :: Int)

        it "Same level split" $ do
          Radix.lookup "thief"
            ( Radix.insert "thither" 3
                . Radix.insert "thief" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (2 :: Int)

        it "Earlier split" $ do
          Radix.lookup "thief"
            ( Radix.insert "trench" 3
               . Radix.insert "thief" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (2 :: Int)

        it "Later split" $ do
          Radix.lookup "thief"
            ( Radix.insert "thistle" 3
               . Radix.insert "thief" 2 $ Radix.singleton "this" 1
            )
            `shouldBe` Just (2 :: Int)

    describe "POSIX rules" $ do
      describe "Short handling" $ do
        it "Optional close" $ do
          run Permute ["foo", "-2bar", "baz"]
            `shouldBe` Right ( set two (Just "bar") defExample, ["foo", "baz"] )

        it "Optional far" $ do
          run Permute ["foo", "-2", "bar", "baz"]
            `shouldBe` Right ( set two (Just "empty") defExample, ["foo", "bar", "baz"] )

        it "Required close" $ do
          run Permute ["foo", "-3bar", "baz"]
            `shouldBe` Right ( set three (Just "bar") defExample, ["foo", "baz"] )

        it "Required far" $ do
          run Permute ["foo", "-3", "bar", "baz"]
            `shouldBe` Right ( set three (Just "bar") defExample, ["foo", "baz"] )

      describe "Short option grouping" $ do
        it "Flags only" $
          run Permute ["foo", "-17", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 $ set seven (Just "other flag") defExample
                             , ["foo", "baz"]
                             )

        it "Unsaturated optional at the end" $
          run Permute ["foo", "-172", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 . set seven (Just "other flag")
                                 $ set two (Just "empty") defExample
                             , ["foo", "baz"]
                             )

        it "Saturated optional at the end" $
          run Permute ["foo", "-172bar", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 . set seven (Just "other flag")
                                 $ set two (Just "bar") defExample
                             , ["foo", "baz"]
                             )

        it "Unsaturated required at the end" $
          run Permute ["foo", "-173"] `shouldSatisfy` isLeft

        it "Saturated required at the end" $
          run Permute ["foo", "-173", "bar", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 . set seven (Just "other flag")
                                 $ set three (Just "bar") defExample
                             , ["foo", "baz"]
                             )

        it "Greedy optional" $
          run Permute ["foo", "-127", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 $ set two (Just "7") defExample
                             , ["foo", "baz"]
                             )

        it "Greedy required" $
          run Permute ["foo", "-137", "baz"]
            `shouldBe` Right ( set one (Just "flag")
                                 $ set three (Just "7") defExample
                             , ["foo", "baz"]
                             )

      it "First non-option quits" $ do
        run RequireOrder ["foo", "-1", "-3", "bar", "baz"]
          `shouldBe` Right (defExample, ["foo", "-1", "-3", "bar", "baz"])

      describe "\"--\" handling" $ do
        it "Ordered" $
          run RequireOrder ["-2", "--", "-3", "bar", "baz"]
            `shouldBe` Right ( set two (Just "empty") defExample
                             , ["-3", "bar", "baz"]
                             )
        it "Permute" $
          run Permute ["foo", "-2", "--", "-3", "bar", "baz"]
            `shouldBe` Right ( set two (Just "empty") defExample
                             , ["foo", "-3", "bar", "baz"]
                             )

    describe "GNU rules" $ do
      describe "Long handling" $ do
        it "Optional close" $ do
          run Permute ["foo", "--option-two=bar", "baz"]
            `shouldBe` Right ( set two (Just "bar") defExample, ["foo", "baz"] )

        it "Optional far" $ do
          run Permute ["foo", "--option-two", "bar", "baz"]
            `shouldBe` Right ( set two (Just "empty") defExample, ["foo", "bar", "baz"] )

        it "Required close" $ do
          run Permute ["foo", "--option-three=bar", "baz"]
            `shouldBe` Right ( set three (Just "bar") defExample, ["foo", "baz"] )

        it "Required far" $ do
          run Permute ["foo", "--option-three", "bar", "baz"]
            `shouldBe` Right ( set three (Just "bar") defExample, ["foo", "baz"] )

    describe "Error sanity" $ do
      describe "Unrecognized" $ do
        it "Short" $
          run Permute ["-u"] `shouldSatisfy` \err ->
            case err of
              Left (Failure 1 "-u" (Unrecognized (Short 'u'))) -> True
              _                                             -> False

        it "Long" $
          run Permute ["--unrecognized"] `shouldSatisfy` \err ->
            case err of
              Left (Failure 1 "--unrecognized" (Unrecognized (Long "unrecognized"))) -> True
              _                                                                      -> False

      describe "Unsaturated" $ do
        it "Short" $
          run Permute ["-3"] `shouldSatisfy` \err ->
            case err of
              Left (Failure 1 "-3" (Unsaturated (Short '3'))) -> True
              _                                               -> False

        it "Long" $
          run Permute ["--option-three"] `shouldSatisfy` \err ->
            case err of
              Left (Failure 1 "--option-three" (Unsaturated (Long "option-three"))) -> True
              _                                                                     -> False

      it "Oversaturated" $
        run Permute ["--option-one=foo"] `shouldSatisfy` \err ->
          case err of
            Left (Failure 1 "--option-one=foo" (Oversaturated "option-one" "foo")) -> True
            _                                                                      -> False

      describe "OptionFailed" $ do
        describe "Short" $ do
          describe "Optional" $ do
            it "Empty" $
              run Permute ["-6"] `shouldSatisfy` \err ->
                case err of
                  Left (Failure 1 "-6" (OptionFailed (Short '6') Nothing "Empty")) -> True
                  _                                                                -> False

            it "Filled" $
              run Permute ["-6Foo"] `shouldSatisfy` \err ->
                case err of
                  Left (Failure 1 "-6Foo" (OptionFailed (Short '6') (Just "Foo") "No Foo")) -> True
                  _                                                                         -> False

          it "Required" $
            run Permute ["-5Foo"] `shouldSatisfy` \err ->
              case err of
                Left (Failure 1 "-5Foo" (OptionFailed (Short '5') (Just "Foo") "No Foo")) -> True
                _                                                                         -> False

        describe "Long" $ do
          describe "Optional" $ do
            it "Empty" $
              run Permute ["--option-six"] `shouldSatisfy` \err ->
                case err of
                  Left (Failure 1 "--option-six" (OptionFailed (Long "option-six") Nothing "Empty")) -> True
                  _                                                                                  -> False

            it "Filled" $
              run Permute ["--option-six=Foo"] `shouldSatisfy` \err ->
                case err of
                  Left (Failure 1 "--option-six=Foo" (OptionFailed (Long "option-six") (Just "Foo") "No Foo")) -> True
                  _                                                                                            -> False

          it "Required" $
            run Permute ["--option-five", "Foo"] `shouldSatisfy` \err ->
              case err of
                Left (Failure 1 "--option-five" (OptionFailed (Long "option-five") (Just "Foo") "No Foo")) -> True
                _                                                                                          -> False

      it "ArgumentFailed" $
        run (ReturnInOrder $ \_ -> Left . (<>) "No ") ["-1", "Foo"] `shouldSatisfy` \err ->
          case err of
            Left (Failure 2 "Foo" (ArgumentFailed "No Foo")) -> True
            _                                                -> False
