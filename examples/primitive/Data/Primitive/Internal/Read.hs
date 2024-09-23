module Data.Primitive.Internal.Read
  ( Tag(..)
  , lexTag
  ) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Tag = FromListTag | FromListNTag

-- Why don't we just use lexP? The general problem with lexP is that
-- it doesn't always fail as fast as we might like. It will
-- happily read to the end of an absurdly long lexeme (e.g., a 200MB string
-- literal) before returning, at which point we'll immediately discard
-- the result because it's not an identifier. Doing the job ourselves, we
-- can see very quickly when we've run into a problem. We should also get
-- a slight efficiency boost by going through the string just once.
lexTag :: ReadP Tag
lexTag = do
  _ <- string "fromList"
  s <- look
  case s of
    'N':c:_
      | isDigit c
      -> fail "" -- We have fromListN3 or similar
      | otherwise -> FromListNTag <$ get -- Skip the 'N'
    _ -> return FromListTag
