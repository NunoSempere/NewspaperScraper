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

-- wrapAll is a wrapper
-- (wrapAll "h1") is a partial function application, which gives another function as an output.
-- the function wrapAll takes 2 arguments, but only one has been given.
-- (+ 2) would also be an example of a function partially applied.
-- mapM_ applies the function (wrappAll "something") to each element in the list, and returns a list with the outputs.

-- Different newspapers encode their headlines differently in html:
-- h1: la razon, la vanguardia
-- h2: el pais, el mundo, la voz de galicia
-- h3: abc, el español.


-- Some auxiliary functions

-- This function ends up outputting to a file. Thus the -> IO()
wrapAll :: String -> String -> IO ()
wrapAll hi url = do
  headlines <- getHeadlines hi url
  myTime <- getZonedTime
  let wrappedHeadlines = ((wrapTime myTime)++(wrapHeadlines headlines))
  -- la url tiene / y caracteres raros que hace que no cree el archivo
  appendFile (urlToFile url) wrappedHeadlines

-- Creating a file which has the special characters :/.? is unnecessarily tricky. We just strip them.
urlToFile :: String -> String
urlToFile url = stripChars ":/.?" url

stripChars :: String -> String -> String
stripChars = filter . flip notElem

wrapHeadlines :: Maybe[String] -> String
wrapHeadlines headlines = unString $ fromJust headlines

-- Using Scalpel
getHeadlines :: String -> URL -> IO(Maybe [String])
getHeadlines hi url = scrapeURL url (texts $ tagSelector hi)

unString :: [String] -> String
unString (x:xs) = x ++ "\n" ++ unString xs
unString [] = ""

-- Using the library import Data.Time
wrapTime :: ZonedTime -> [Char]
wrapTime myTime = ("\n"++"DATE=" ++ (formatTime defaultTimeLocale "%T, %F (%Z)" myTime)++"\n")

-- Characters to take care of: Á, É, Í, Ó, Ú. á,é,í,ó,ú, ñ, ü,"", ¡,!,¿?


