

data DExpr = DSymbol String
    | DList [DExpr]
    | DLParen
    | DRParen
    deriving Show


getTokensFromCLI = words <$> getLine

