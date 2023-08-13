import Text.ParserCombinators.Parsec


data DExpr = DSymbol String
    | DInt      Int
    | DFloat    Float
    | DList     [DExpr]
    | DLParen
    | DRParen
    deriving Show


getTokensFromCLI = words <$> getLine


-- Parse symbols (words). Start with a letter but may include digits
parseSymbol :: Parser DExpr
parseSymbol = do
    first   <- letter
    rest    <- many (letter <|> digit)
    let symbol = first : rest
    return (DSymbol symbol)


parseInt :: Parser DExpr
--parseInt = do
--    num     <- many digit
--    return  (DInt (read num :: Int))
parseInt = return (DInt (read many digit :: Int))

parseFloat :: Parser DExpr
parseFloat = do
    num     <- many digit
    char '.'
    rest    <- many digit
    let floatnum = num ++ "." ++ rest
    return  (DFloat (read floatnum :: Float))


parseNumber :: Parser DExpr
parseNumber = parseInt <|> parseFloat


parseExpr :: Parser DExpr
parseExpr = parseSymbol
    <|> parseNumber


parseLine :: String -> String
parseLine input = case parse parseExpr "DDPreter" input of
    Left err        -> "No match: " ++ show err
    Right parsed    -> "Parsed everything"

