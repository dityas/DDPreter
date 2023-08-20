import DDParser


getPromptInput :: String -> IO String
getPromptInput prompt = putStr prompt >> getLine


-- Need to figure out how to present a prompt till the input is gramatically
-- valid to be parsed.
--
--

replOnce :: IO DExpr
replOnce = parseLine <$> getPromptInput ">> "
