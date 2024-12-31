module Parse where

import Data.Char

data Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser (map (\(x,s) -> (f x,s)) . p)

instance Applicative Parser where
    pure x = Parser (\s -> [(x,s)])

    p <*> q = Parser $ \s -> [(f x, s'') | (f, s' ) <- parse p s, (x, s'') <- parse q s'] 

instance Alternative Parser where
    empty   = Parser $ \s -> []
    p <|> q = Parser $ \s ->
              case parse p s of
                []  -> parse q s
                res -> res
               
instance Monad Parser where
    p >>= f   = Parser( \s -> concat [parse (f x) s' | (x,s') <- parse p s] )


runParser :: Parser a -> String -> a
runParser m s = case parse m s of
                  [(res,[])] -> res
                  [(_,rs)]   -> error "Parser did not consume entire string."
                  _          -> error "Parse error"


-- Match any non-empty string, and return the first char
item :: Parser Char
item = Parser $ \s ->
       case s of
         [] -> []
         (c:cs) -> [(c,cs)]

failure = Parser (\s -> [])

combine p q = Parser (\s -> parse p s ++ parse q s)


satisfy p = do
  c <- item
  if p c
  then return c
  else failure

-- end core

oneOf s = satisfy (flip elem s)

chainl p op a = (p `chainl1` op) <|> return a

p `chainl1` op = do {a <- p; rest a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                   <|> return a

char c = satisfy (c ==)

-- where to define read_int, read_double?
natural :: Parser Int
natural = read <$> some digit

string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token p = do { a <- p; spaces ; return a}

reserved s = token (string s)

spaces = many $ satisfy isSpace

digit = satisfy isDigit

number = do
  s <- string "-" <|> return []
  cs <- some digit
  return (read (s++cs) :: Int)

parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

sepBy1 p sep = do { x <- p ; rest x }
    where
      rest x = (do sep
                   xs <- sepBy1 p sep
                   return (x:xs))
               <|> return [x]

sepBy p sep = sepBy1 p sep <|> return []

option x p = p <|> return x

optional p = (p >> return ()) <|> return ()

optionMaybe p = (Just <$> p) <|> (return Nothing)

alphaNum c = satisfy isAlphaNum

-- a double is -integer[.integer][einteger]
parse_double = do s <- option [] sign
                  i1 <- some digit
                  i2 <- option [] fraction
                  i3 <- option [] exponent
                  let word = s++i1++i2++i3
                  return (read word :: Double)
    where fraction = do string "."
                        n <- some digit
                        return ('.':n)
          exponent = do string "e"
                        s <- option [] sign
                        n <- some digit
                        return ('e':(s++n))
          sign = (\c->[c]) <$> oneOf "+-"

