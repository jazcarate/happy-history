module Search where

import           RIO
import           RIO.List                       ( sortOn )
import qualified RIO.Text                      as T

-- | Filter a list of texts matching by order
-- 
-- >>> search "git" ["git add .", "goto it", "foo"] ">" "<" id
-- [[yes "git", no " add ."], [yes "g", no "oto ", yes "it"]]
search
  :: Text          -- ^ Pattern.
  -> [Text]        -- ^ The list of values containing the text to search in.
  -> [Result Text] -- ^ The list of results, sorted, highest score first.
search pattern texts =
  sortOn (Down . longestYes) $ mapMaybe (matchOn pattern) texts

data Match a = Match Bool a
type Result a = [Match a]

yes :: a -> Match a
yes = Match True

no :: a -> Match a
no = Match False

match :: (a -> b) -> (a -> b) -> Match a -> b
match ifYes ifNo (Match b a) = (if b then ifYes else ifNo) a

matchOn :: Text -> Text -> Maybe (Result Text)
matchOn needle haystack = compact <$> matchOn' needle haystack

compact :: [Match Text] -> Result Text
compact = merge . removeEmpty

merge :: (Semigroup a) => [Match a] -> [Match a]
merge (x1@(Match b1 x) : x2@(Match b2 y) : xs) =
  if b1 == b2 then merge $ Match b1 (x <> y) : xs else x1 : merge (x2 : xs)
merge [x] = [x]
merge []  = []

removeEmpty :: (Eq a, Monoid a) => [Match a] -> [Match a]
removeEmpty = filter $ match isNotEmpty isNotEmpty
  where isNotEmpty x = mempty /= x

matchOn' :: Text -> Text -> Maybe [Match Text]
matchOn' needle haystack = case T.uncons needle of
  Just (n, ns) ->
    let (start, end) = T.break (== n) haystack
    in  case T.uncons end of
          Just (found, rest) ->  -- If it is found, then `end` has at least the char found
            (\acc -> no start : yes (T.singleton found) : acc)
              <$> matchOn ns rest
          Nothing -> Nothing
  Nothing -> Just $ [no haystack]

longestYes :: Result Text -> Int
longestYes = foldl' max 0 . (match T.length (const 0) <$>)
