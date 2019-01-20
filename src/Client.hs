-- | The Client module
module Client( 
      client,
      clientloop,
      fire,
      wait,
      fieldsetting,
      forbiddentuples,
      memberInSet,
      check'
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



--        Qsem1   Qsem2   Feld                   (Down),(UP)          Player button
-- | The client that implements the clientside gamelogic
client :: QSem -> QSem -> IORef [[(Int,Int)]] -> IORef [(Int,Int)] -> IORef Bool -> IORef Bool -> [IORef [(Int,Int)]] -> IORef Bool -> IO ()
client qsem1 qsem2 field coords play allowed2fire vars bool_left = do
                                         fieldsetting qsem1 qsem2 field coords play -- Feldbelegung
                                        -- putStrLn "--- played clicked ---"
                                         server <- socket AF_INET Stream 0
                                         host <- inet_addr "127.0.0.1"
                                         let sktAddr = SockAddrInet 4242 host
                                         connect server sktAddr
                                         fff <- readIORef field
                                         let playerfield = show . tail $ fff
                                         send server playerfield  -- Sende Feldbelegung zum server
                                         wer_faengt_an <- recv server 4000
                                         case wer_faengt_an of
                                                    "0" -> clientloop 0 qsem1 qsem2 playerfield coords server allowed2fire vars bool_left -- Gegner darf zuerst feuern
                                                    "1" -> clientloop 1 qsem1 qsem2 playerfield coords server allowed2fire vars bool_left -- Spieler darf zuerst feuern
-- | Implements the logic of the main game
clientloop :: Int -> QSem -> QSem -> [Char] -> IORef [(Int,Int)] -> Socket -> IORef Bool -> [IORef [(Int,Int)]] -> IORef Bool -> IO ()
clientloop 0 sem1 sem2 field downpoint server allowed2fire [nieten_left , treffer_left , versenkt_left , nieten_right , treffer_right , versenkt_right] bool_left = do
                                                   wait server nieten_left treffer_left versenkt_left bool_left
                                                   fire sem1 sem2 downpoint server nieten_right treffer_right versenkt_right allowed2fire
                                                   clientloop 0 sem1 sem2 field downpoint server allowed2fire [nieten_left , treffer_left , versenkt_left , nieten_right , treffer_right , versenkt_right] bool_left

clientloop 1 sem1 sem2 field downpoint server allowed2fire [nieten_left , treffer_left , versenkt_left , nieten_right , treffer_right , versenkt_right] bool_left = do
                                                   fire sem1 sem2 downpoint server nieten_right treffer_right versenkt_right allowed2fire
                                                   wait server nieten_left treffer_left versenkt_left bool_left
                                                   clientloop 1 sem1 sem2 field downpoint server allowed2fire [nieten_left , treffer_left , versenkt_left , nieten_right , treffer_right , versenkt_right] bool_left
-- | Is called when the player is allowed to fire
fire :: QSem -> QSem -> IORef [(Int,Int)] -> Socket -> IORef [(Int,Int)] -> IORef [(Int,Int)] -> IORef [(Int,Int)] -> IORef Bool -> IO ()
fire qsem1 qsem2 downpoint server nieten_right treffer_right versenkt_right allowed2fire = do
                writeIORef allowed2fire True
                waitQSem qsem1
                shoot <- readIORef downpoint
                let str = Data.List.filter (/= ',') (show.head$shoot)
                send server str
                response <- recv server 4000
                let lst = splitRegex (mkRegex " ") response
                case lst !! 0 of
                     "0" -> do
                              let t = tuple (tail.init$ (lst !! 1))
                              tt <- readIORef nieten_right
                              writeIORef nieten_right (tt ++ [t])
                              writeIORef allowed2fire False
                              signalQSem qsem2
                              --writeIORef bool_left True
                     "1" -> do
                              let t = tuple (tail.init$ (lst !! 1))
                              tt <- readIORef treffer_right
                              writeIORef treffer_right (tt ++ [t])
                              writeIORef allowed2fire True
                              signalQSem qsem2
                              --writeIORef bool_left True
                              fire qsem1 qsem2 downpoint server nieten_right treffer_right versenkt_right allowed2fire
                     "2" -> do
                              let t = tuple (tail.init$ (lst !! 1))
                              tt <- readIORef versenkt_right
                              ff <- readIORef treffer_right
                              let lst_treffer = ff ++ [t]
                              writeIORef versenkt_right (tt ++ lst_treffer)
                              writeIORef allowed2fire True
                              signalQSem qsem2
                              fire qsem1 qsem2 downpoint server nieten_right treffer_right versenkt_right allowed2fire
              where tuple :: [Char] -> (Int,Int)
                    tuple [a,b,c]  | a == '1' && b == '0' = (10,digitToInt c)
                                   | b == '1' && c == '0' = (digitToInt a,10)
                    tuple [a,b,c,d] = (10,10)
                    tuple [a,b] = (digitToInt$a,digitToInt$b)
-- | Is called when the player is waiting for the enemeys choice
wait :: Socket -> IORef [(Int,Int)] -> IORef [(Int,Int)] -> IORef [(Int,Int)] -> IORef Bool -> IO ()
wait server nieten_left treffer_left versenkt_left bool_left = do
                                                                 response <- recv server 4000
                                                                 let lst = splitRegex (mkRegex " ") response
                                                                 case head lst of   -- lst !! 0
                                                                           "0" -> do
                                                                                     let t = tuple (tail.init$ (lst !! 1))
                                                                                     tt <- readIORef nieten_left
                                                                                     writeIORef nieten_left (tt ++ [t])
                                                                                     writeIORef bool_left True
                                                                           "1" -> do
                                                                                     let t = tuple (tail.init$ (lst !! 1))
                                                                                     tt <- readIORef treffer_left
                                                                                     writeIORef treffer_left (tt ++ [t])
                                                                                     writeIORef bool_left True
                                                                                     wait server nieten_left treffer_left versenkt_left bool_left
                                                                           "2" -> do
                                                                                     let t = tuple (tail.init$ (lst !! 1))
                                                                                     tt <- readIORef versenkt_left
                                                                                     ff <- readIORef treffer_left
                                                                                     let lst_treffer = ff ++ [t]
                                                                                     writeIORef versenkt_left (tt ++ lst_treffer)
                                                                                     writeIORef bool_left True
                                                                                     wait server nieten_left treffer_left versenkt_left bool_left
                                                                 where tuple :: [Char] -> (Int,Int)
                                                                       tuple [a,b,c]  | a == '1' && b == '0' = (10,digitToInt c)
                                                                                      | b == '1' && c == '0' = (digitToInt a,10)
                                                                       tuple [a,b,c,d] = (10,10)
                                                                       tuple [a,b] = (digitToInt$a,digitToInt$b)
-- | Is called at the beginning of the game
fieldsetting :: QSem -> QSem -> IORef [[(Int,Int)]] -> IORef [(Int,Int)] -> IORef Bool -> IO ()
fieldsetting qsem1 qsem2 field coords play = do
                                               waitQSem qsem1    -- wird entsperrt bei MouseUP Event und Play-Click
                                               playclicked <- readIORef play
                                               ships <- readIORef field
                                               if playclicked && (length . tail $ ships) == 10 then do
                                                                                                       signalQSem qsem2
                                                                                                       return ()
                                               else do
                                                      [(x1,y1),(x2,y2)] <- readIORef coords
                                                      if (x1 /= x2 && y1 /= y2) then do
                                                                                       signalQSem qsem2
                                                                                       fieldsetting qsem1 qsem2 field coords play 
                                                      else do
                                                            let xs = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
                                                            if (length xs > 5 || length xs == 1) then do
                                                                                                        signalQSem qsem2
                                                                                                        fieldsetting qsem1 qsem2 field coords play
                                                            else do
                                                                   if check' xs (tail ships) then do
                                                                                                     signalQSem qsem2
                                                                                                     fieldsetting qsem1 qsem2 field coords play
                                                                   else do
                                                                          let set = forbiddentuples (tail ships)
                                                                          if memberInSet xs set then do
                                                                                                     signalQSem qsem2
                                                                                                     fieldsetting qsem1 qsem2 field coords play
                                                                          else do
                                                                                 writeIORef field (ships ++ [xs])
                                                                                 signalQSem qsem2
                                                                                 fieldsetting qsem1 qsem2 field coords play
-- | Creates a Set of forbidden fields
forbiddentuples :: [[(Int,Int)]] -> Set (Int,Int)
forbiddentuples xss = Set.fromList . concat $ [[(a-1,b),(a,b-1),(a+1,b),(a,b+1),(a,b)] | xs <- xss, (a,b) <- xs]

-- | Checks if the players choice is a member of the Set of forbidden fields
memberInSet :: [(Int,Int)] -> Set (Int,Int) -> Bool
memberInSet xs set = or [Set.member a set | a <- xs]
-- | Checks if the maximal number of fields is valid
check' :: [(Int,Int)] -> [[(Int,Int)]] -> Bool
check' xs xss = if check'' xsss then True
                else False
                where xsss = [t | t <- xss, length t == length xs]
                      check'' :: [[(Int,Int)]] -> Bool
                      check'' [] = False
                      check'' [[a,b,c,d,e]] = True
                      check'' [[a,b,c,d],[e,f,g,h]] = True
                      check'' [[a,b,c],[d,e,f],[g,h,i]] = True
                      check'' [[a,b],[c,d],[e,f],[g,h]] = True
                      check'' _ = False
