module DDParser where
-- Bulk of this code is based on the Haskell Scheme tutorial
-- Ref: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
--
-- Update: I really hate the do notation. Feels a lot like any other language.
-- So I'll try to avoid the do notation as far as possible.

import Text.ParserCombinators.Parsec hiding (spaces)


data DExpr = DSymbol String
    | DInt      Int
    | DFloat    Float
    | DList     [DExpr]
    deriving Show


spaces :: Parser ()
spaces = skipMany1 space

number = many1 digit


-- Parse symbols (words). Start with a letter but may include digits
parseSymbol :: Parser DExpr
parseSymbol = fmap DSymbol $ (:) <$> first <*> rest
    where 
        first   = letter
        rest    = many (letter <|> digit)


parseInt :: Parser DExpr
parseInt = (DInt . read) <$> number


parseFloat :: Parser DExpr
parseFloat = (DFloat . read) <$> ((++) <$> number <*> decimal)
    where decimal = (:) <$> char '.' <*> number

parseList :: Parser DExpr
parseList = fmap DList $ char '(' *> (sepBy parseExpr spaces) <* char ')'


-- Apparently Parsec does not backtrack by default. So I use try here. I have
-- no idea how try does what it does though.
parseNumber :: Parser DExpr
parseNumber = try parseFloat <|> parseInt


parseExpr :: Parser DExpr
parseExpr = parseSymbol
    <|> parseNumber
    <|> parseList


parseLine :: String -> DExpr
parseLine input = case parse parseExpr "DDPreter" input of
    Left err        -> DList $ [DSymbol "error", DSymbol $ show err]
    Right parsed    -> parsed

