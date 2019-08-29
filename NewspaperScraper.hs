module Main where

import Text.HTML.Scalpel
import Data.Maybe
import Data.Typeable
import System.IO
import Data.Time
import System.Environment

main :: IO ()
main = do
  mapM_ (wrapAll "h1") ["https://lavanguardia.com"]
  mapM_ (wrapAll "h2") ["https://www.larazon.es","https://elpais.com/", "https://www.elmundo.es/", "https://www.lavozdegalicia.es/"] 
  mapM_ (wrapAll "h3") ["https://www.abc.es/", "https://www.elespanol.com/"]


-- h1: la razon, ?la vanguardia
-- h2: el pais, el mundo, la voz de galicia
-- h3: abc, el español.


-- Some auxiliary functions

wrapAll :: String -> String -> IO ()
wrapAll hi url = do
  headlines <- getHeadlines hi url
  myTime <- getZonedTime
  let wrappedHeadlines = ((wrapTime myTime)++(wrapHeadlines headlines))
  -- la url tiene / y caracteres raros que hace que no cree el archivo
  appendFile (urlToFile url) wrappedHeadlines

urlToFile :: String -> String
urlToFile url = stripChars ":/.?" url

stripChars :: String -> String -> String
stripChars = filter . flip notElem

wrapHeadlines :: Maybe[String] -> String
wrapHeadlines headlines = unString $ fromJust headlines

getHeadlines :: String -> URL -> IO(Maybe [String])
getHeadlines hi url = scrapeURL url (texts $ tagSelector hi)

unString :: [String] -> String
unString (x:xs) = x ++ "\n" ++ unString xs
unString [] = ""

wrapTime :: ZonedTime -> [Char]
wrapTime myTime = ("\n"++"DATE=" ++ (formatTime defaultTimeLocale "%T, %F (%Z)" myTime)++"\n")

-- Cosas que tener en cuenta: Á, É, Í, Ó, Ú. á,é,í,ó,ú, ñ, ü,"", ¡,!,¿?


