module Main (
	main,
) where

import Network.Socket
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Maybe
import Data.String.Utils
import qualified Data.Map as M

main = do
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bindSocket sock (SockAddrInet 1122 iNADDR_ANY)
	listen sock 2
	mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do
	conn <- accept sock
	forkIO (runConn conn)
	mainLoop sock

stripfirst :: String -> String -> String
stripfirst s a = drop (length s) a

type DDPPXP = (String, M.Map String String)

skipLit :: String -> DDPPXP -> Maybe DDPPXP
skipLit s (a,m) = if startswith s a then return (stripfirst s a, m) else Nothing

saveLit :: String -> DDPPXP -> Maybe DDPPXP
saveLit s (a,m) = if (length lst) > 0 then return (unwords $ tail lst, M.insert s (head lst) m) else Nothing
	where lst = words a

saveLitToNL :: String -> DDPPXP -> Maybe DDPPXP
saveLitToNL s (a,m) = return ("", M.insert s (a) m)

parse :: String -> Maybe DDPPXP
parse s = return (s, M.empty)  >>= saveLit "A" >>= skipLit "say " >>= saveLitToNL "X"

mkresponse :: DDPPXP -> String
mkresponse (_, m) = a ++ ": " ++ x ++ " " ++ x
	where (a,x) = (fromJust $ M.lookup "A" m, fromJust $ M.lookup "X" m)

runConn :: (Socket, SockAddr) -> IO()
runConn (sock, _) = do
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering
	lA <- hGetLine hdl
	lB <- hGetLine hdl
	let lineA = parse lA
	if lineA /= Nothing then 
		hPutStrLn hdl  $ mkresponse $ fromJust lineA
			else
		hPutStrLn hdl "Error in Line 1\n"
	let lineB = parse lB
	if lineB /= Nothing then 
		hPutStrLn hdl  $ mkresponse $ fromJust lineB
			else
		hPutStrLn hdl "Error in Line 2\n"
	hClose hdl
