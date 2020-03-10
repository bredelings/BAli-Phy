module Parse where

import Data.Char

data Parser a = Parser (String -> [(a,String)])

parse (Parser method) s = method s
parse (IOAndPass p f) s = concat [parse (f x) s' | (x,s') <- parse p s]
parse (IOReturn x) s = [(x,s)]

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

p <|> q = Parser $ \s ->
          case parse p s of
            []  -> parse q s
            res -> res

some v = some_v
    where
      many_v = some_v <|> return []
      some_v = do
        x <- v
        xs <- many_v
        return (x:xs)
               
many v = many_v
    where
      many_v = some_v <|> return []
      some_v = do
        x <- v
        xs <- many_v
        return (x:xs)
               
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

-- This should be defined in Data.Functor
f <$> x = do {result <- x ; return (f result)}
-- This should be defined .. where?  Applicative?             
f <*> x = do {fresult <- f ; xresult <- x ; return (fresult xresult)}

-- where to define read_int, read_double?
natural = read_int <$> some digit

string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token p = do { a <- p; spaces ; return a}

reserved s = token (string s)

spaces = many $ oneOf " \n\r"

digit = satisfy isDigit

number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read_int (s++cs)

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
                  return (read_double word)
    where fraction = do string "."
                        n <- some digit
                        return ('.':n)
          exponent = do string "e"
                        s <- option [] sign
                        n <- some digit
                        return ('e':(s++n))
          sign = (\c->[c]) <$> oneOf "+-"

