module Payer1 where


import Data.List
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BytCh
import qualified Data.ByteString.Lazy.Char8 as BytChLazy
import qualified System.IO as SysIO
import qualified Data.CaseInsensitive as CI

type Cord = (String, String, String)
type PirmMap = [(Cord)]
type Ats = (String, String)

message :: String
message = answer "[{\"x\": 0, \"y\": 0, \"v\": \"x\"}, {\"x\": 2, \"y\": 1, \"v\": \"o\"}, {\"x\": 0, \"y\": 2, \"v\": \"x\"}, {\"x\": 2, \"y\": 2, \"v\": \"o\"}, {\"x\": 0, \"y\": 1, \"v\": \"x\"}, {\"x\": 1, \"y\": 1, \"v\": \"o\"}, {\"x\": 1, \"y\": 2, \"v\": \"x\"}, {\"x\": 2, \"y\": 0, \"v\": \"o\"}, {\"x\": 1, \"y\": 0, \"v\": \"x\"}]"

xar :: String
xar = "012"

yar :: String
yar = "012"

--Main funkcija kvietimui
main :: IO ()
main = do
	postA "[]"
	SysIO.putStrLn "GG"

url1 = "http://tictactoe.homedir.eu/game/mako1416/player/1"

toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

postA msg = do
    let opts = defaults & header (toHeaderName "Content-Type") .~ [BytCh.pack "application/json+list"]
    let msg' = answer msg
    r <- postWith opts url1 $ BytCh.pack (msg')
    if answer msg' == "GG"
        then return()
        else getA

getA = do
    let opts2 = defaults & header (toHeaderName "Accept") .~ [BytCh.pack "application/json+list"]
    r <- getWith opts2 url1
    let msg = BytChLazy.unpack(r ^. responseBody)
    if answer msg == "GG"
        then return()
        else postA msg


--patikrinam ar duotas stringas ne tuscias jei ne kvieciam funkcija kas rastu ats
answer :: String -> String
answer "[]" = "[{\"x\": 0, \"y\": 0, \"v\": \"x\"}]"
answer d  = 
	let
		call = addToList(removeBrackets(removeSpaces d)) []
		last = findlast call
		(ats, ats2) = perx call xar
		b = takeWhile (/= ']') d
		at = concat [b, ", {\"x\": ", ats, ", \"y\": ", ats2, ", \"v\": \"", last, "\"}]"]
	in if (ats == "n" && ats2 == "n")
	then "GG"
	else at

--funkcija pašalinti laužtinius skliaustus(brackets)
removeBrackets :: String -> String
removeBrackets move = 
	let 
		bb = drop 1 move	
		bbb = takeWhile (/= ']') bb
		
	in bbb

--pasalina tarpus
removeSpaces :: String -> String
removeSpaces str = filter(/=' ') str	

first :: Cord -> String
first (x,_,_) = x

second :: Cord -> String
second (_,x,_) = x

third :: Cord -> String
third (_,_,x) = x


addToList :: String -> PirmMap -> PirmMap
addToList [] list = list
addToList ad list =
	let
        (move, rest) = readMoves ad
    in addToList rest (move : list)
	
	
readMoves :: String -> (Cord, String) -- (ejimas, likusi stringo dalis)
readMoves mo = 
	let
	mov = takeWhile (/= '}') mo
	--tailas = drop 19 mo
	mov2 = drop 5 mo
	pirm = [mov2 !! 0]
	mov3 = drop 6 mov2
	antr = [mov3 !! 0]
	mov4 = drop 7 mov3
	trec = [mov4 !! 0]
	tailas = drop 4 mov4
	in ((pirm, antr, trec), tailas)	
	


findlast :: PirmMap -> String
findlast [] = ""
findlast a =
		let
		pask = head a
		laste = third pask
		in if (laste == "o") 
		then "x"
		else "o"
	
	
		
		
perx :: PirmMap -> String -> Ats 
perx a "" = ("n", "n")
perx a b =
			let
			ordx = [b !! 0]
			tailas = tail b
			z = pery a yar ordx
			in if (z /= "n") 
				then (ordx, z)
			else perx a tailas
			
pery :: PirmMap -> String -> String -> String
pery a "" x = "n"
pery a b x =
			let
			ordy = [b !! 0]
			tailas = tail b
			z = pereitiPerMapa a x ordy
			in if (z == True) 
				then ordy
				else pery a tailas x
		
		
	
pereitiPerMapa :: PirmMap -> String -> String -> Bool
pereitiPerMapa [] x y = True
pereitiPerMapa map x y = 
		let
			pirmas = head map
			tailas = tail map
			xas = x
			yas = y
			
		in if ((xas == first pirmas) && (yas == second pirmas))
				then False
				else pereitiPerMapa tailas xas yas	