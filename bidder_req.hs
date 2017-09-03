import Network.HTTP
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Data.Maybe

-- 1. Perform a basic HTTP get request and return the body
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

main = mapM_ showSingleMetrics servers

-- servers = "nyc-b9" : "nyc-b8" : []
servers = ["nyc-b" ++ show x | x <- [1..9]]

showSingleMetrics server = do
  metrics <- get ("http://" ++ server ++ ".merchenta.com:3333/metrics/metrics")
  let parsed = decode metrics :: Result JSValue
  putStrLn server
  putStrLn (unwords $ params parsed)
  putStrLn ""

params :: Result JSValue -> [String]
params (Ok m) = [
  getString ["com.merchenta.bidder.networks.ExchangeServlet", "zero bid processing time", "duration", "p95"]
  getString ["com.merchenta.bidder.networks.ExchangeServlet", "non-zero bid processing time", "duration", "p95"]
  ]
  where getString = \path -> fromJust $ findJsObject path m
params (Error e) = [e]


findJsObject :: [String] -> JSValue -> Maybe String
findJsObject [] _ = Nothing
findJsObject (x:[]) (JSObject object) = case (get_field object x) of Just res -> Just (encode res)
                                                                     _ -> Nothing
findJsObject (x:xs) (JSObject object) = (get_field object x) >>= (findJsObject xs)
findJsObject _ _ = Nothing
