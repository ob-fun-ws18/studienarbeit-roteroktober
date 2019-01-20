module Server(
        server,
        gameloop,
        checkfireplayer1,
        checkfireplayer2
)
where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import Data.IORef
import System.Random
import Control.Concurrent.QSem
import Data.Set as Set
import Data.List
import Text.Regex
import Data.Char


server :: IO ()
server = do
          serversocket <- socket AF_INET Stream 0
          setSocketOption serversocket ReuseAddr 1   -- make socket immediately reusable - eases debugging.
          bind serversocket (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
          listen serversocket 3
          (sock1,sockaddr1) <- accept serversocket     -- accept a connection and handle it
          (sock2,sockaddr2) <- accept serversocket
          str1 <- recv sock1 4000
          str2 <- recv sock2 4000
          let player1 = Data.List.filter (/= ',') (init.tail$str1)
          let player2 = Data.List.filter (/= ',') (init.tail$str2)
          putStrLn player1
          putStrLn player2
          send sock1 "1"
          send sock2 "0"
          gameloop player1 player2 sock1 sock2

gameloop :: [Char] -> [Char] -> Socket -> Socket -> IO ()
gameloop str1 str2 sock1 sock2 = do
                                    field2 <- checkfireplayer1 str1 str2 sock1 sock2
                                    field1 <- checkfireplayer2 str1 str2 sock1 sock2
                                    --putStrLn (field2 ++ "\n +++++ \n" ++ field1)
                                    gameloop field1 field2 sock1 sock2

checkfireplayer1 :: [Char] -> [Char] -> Socket -> Socket -> IO [Char]
checkfireplayer1 str1 str2 sock1 sock2 = do
                                 fire1 <- recv sock1 4000
                                 if matchRegex (mkRegex fire1) str2 /= Nothing
                                  then do
                                        let field2 = concat $ (splitRegex (mkRegex ("\\("++ (init.tail$fire1) ++"\\)")) str2)
                                        if matchRegex (mkRegex "\\[\\]") field2 /= Nothing
                                          then do 
                                                 send sock1 ("2 " ++ fire1)
                                                 send sock2 ("2 " ++ fire1)
                                                 let newfield2 = concat $ (splitRegex (mkRegex "\\[\\]") field2)
                                                 checkfireplayer1 str1 newfield2 sock1 sock2
                                        else do
                                                 send sock1 ("1 " ++ fire1)
                                                 send sock2 ("1 " ++ fire1)
                                                 checkfireplayer1 str1 field2 sock1 sock2
                                 else do
                                         send sock1 ("0 " ++ fire1)
                                         send sock2 ("0 " ++ fire1)
                                         return str2

checkfireplayer2 :: [Char] -> [Char] -> Socket -> Socket -> IO [Char]
checkfireplayer2 str1 str2 sock1 sock2 = do
                                        fire2 <- recv sock2 4000
                                        if matchRegex (mkRegex fire2) str1 /= Nothing
                                         then do
                                             let field1 = concat $ (splitRegex (mkRegex ("\\("++( init.tail$fire2 )++"\\)")) str1)
                                             if matchRegex (mkRegex "\\[\\]") field1 /= Nothing
                                                 then do 
                                                        send sock2 ("2 " ++ fire2)
                                                        send sock1 ("2 " ++ fire2)
                                                        let newfield1 = concat $ (splitRegex (mkRegex "\\[\\]") field1)
                                                        checkfireplayer2 newfield1 str2 sock1 sock2
                                              else do
                                                      send sock2 ("1 " ++ fire2)
                                                      send sock1 ("1 " ++ fire2)
                                                      checkfireplayer2 field1 str2 sock1 sock2
                                         else do
                                                send sock1 ("0 " ++ fire2)
                                                send sock2 ("0 " ++ fire2)
                                                return str1