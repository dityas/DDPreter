

data DExpr = DSymbol String
    | DList [DExpr]
    deriving Show


parseLine :: String -> (Int, String, String)
parseLine "" = (0, "", "")
parseLine _ = (-1, "", "")

parseLinePartial :: (Int, String, String) -> (Int, String, String)
parseLinePartial (parens, "", other) = (parens, other, "")
