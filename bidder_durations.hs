import Network.HTTP
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Data.Maybe
import Data.Ratio

-- 1. Perform a basic HTTP get request and return the body
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

main = mapM_ showSingleMetrics servers

servers = "lon-b9" : []
-- servers = ["nyc-b" ++ show x | x <- [1..9]]

showSingleMetrics server = do
  metrics <- get ("http://" ++ server ++ ".merchenta.com:3333/metrics/metrics")
  let parsed = decode metrics :: Result JSValue
  putStrLn server
  putStrLn $ unlines $ map toString (findDurations [] (resultToJsValue parsed))
  putStrLn ""

toString :: ([String], String, Double) -> String
toString (path, unit, duration) = show path ++ " " ++ unit ++ " " ++ show duration ++ (if unit /= "\"milliseconds\"" || duration > 1 then " !!!!" else "")

v :: Double
v = -1

findDurations :: [String] -> JSValue -> [ ([String], String, Double) ]
findDurations path (JSObject object) = case (get_field object "duration") of Just (JSObject dur) -> (reverse path, fromMaybe "" $ fmap encode $ get_field dur "unit", fromMaybe v $ fmap getInteger $ get_field dur "p75") : []
                                                                             _ -> concatMap (\x-> findDurations (fst x : path) (snd x)) (fromJSObject object)
findDurations _ _ = []

getInteger :: JSValue -> Double
getInteger (JSRational _ a) = (fromIntegral $ numerator a) / (fromIntegral $ denominator a)
getInteger _ = error "not json number"

resultToJsValue :: Result JSValue -> JSValue
resultToJsValue (Ok j) = j
resultToJsValue (Error e) = error "not valid json"
