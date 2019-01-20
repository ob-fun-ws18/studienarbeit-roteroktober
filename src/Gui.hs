module Gui(
        setup
)
where

import Client
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
    
import Control.Monad (forever)
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
import Data.Ord

bars  = 4
beats = 4
defaultBpm = 120

bpm2ms :: Int -> Int
bpm2ms bpm = ceiling $ 1000*60 / fromIntegral bpm

setup :: Window -> UI ()
setup w = do
            coords <- liftIO . newIORef $ [(0,0)]
            semaphore <- liftIO . newQSem $ 0
            semaphore2 <- liftIO . newQSem $ 0
            player_id <- liftIO (randomIO :: IO Int)
            field <- liftIO . newIORef $ [[(player_id,player_id)]]
            playclicked <- liftIO . newIORef $ False
            allowed2fire <- liftIO . newIORef $ False
            nieten_left <- liftIO . newIORef $ [(0,0)]
            treffer_left <- liftIO . newIORef $ [(0,0)]
            versenkt_left <- liftIO . newIORef $ [(0,0)]
            nieten_right <- liftIO . newIORef $ [(0,0)]
            treffer_right <- liftIO . newIORef $ [(0,0)]
            versenkt_right <- liftIO . newIORef $ [(0,0)]
            bool_left <- liftIO . newIORef $ False
            let vars = [ nieten_left , treffer_left , versenkt_left , nieten_right , treffer_right , versenkt_right ]
            liftIO . forkIO $ (client semaphore semaphore2 field coords playclicked allowed2fire vars bool_left) -- fieldsetting qsem1 qsem2 field coords play
            id <- liftIO (randomIO :: IO Int)
            st <- UI.div # set text "d" # set style [("width","50px"),("height","30px"),("text-align","center"),("visibility","hidden")]
            st_left <- UI.div # set text "d" # set style [("width","50px"),("height","30px"),("text-align","center"),("visibility","hidden")]
            st_right <- UI.div # set text "d" # set style [("width","50px"),("height","30px"),("text-align","center"),("visibility","hidden")]
            sp <- UI.span # set style [("margin-bottom","110px"),("width","10px"),("margin-left","205px")]
            let a = [ UI.div # set text (show bb) # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- [1..10]]
            let a_left = [ UI.div # set text (show bb) # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- [1..10]]
            let a_right = [ UI.div # set text (show bb) # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- [1..10]]
            let b = [ UI.div # set text (bb:"") # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- ['a'..'j']]
            let b_left = [ UI.div # set text (bb:"") # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- ['a'..'j']]
            let b_right = [ UI.div # set text (bb:"") # set style[("text-align","center"),("width","50px"),("height","30px"),("background-color","blue")] | bb <- ['a'..'j']]
            let cc =  [element st] ++ b
            let cc_left = [element st_left] ++ b_left
            let cc_right = [element st_right] ++ b_right
            title <- UI.div # set text "Jagd auf Roter Oktober" # set style [("background-color","yellow"),("text-align","center"),("font-size","28px"),("margin-bottom","5px")]
            canvas <- UI.canvas # set UI.width 500 # set UI.height 300 # set style [("background-color","cyan")]
            field_left <- UI.canvas # set UI.width 500 # set UI.height 300 # set style [("background-color","cyan")]
            field_right <- UI.canvas # set UI.width 500 # set UI.height 300 # set style [("background-color","cyan")]
            drawcanvas canvas
            reset <- UI.button # set text "reset" # set style [("border-radius","5px"),("width","8%")]
            play <- UI.button # set text "play"  # set style [("border-radius","5px"),("width","8%")]
            anzeige <- UI.canvas # set UI.width 200 # set UI.height 300 # set style [("background-color","#eeeeee"),("margin-left","10px")] # set UI.fillStyle (UI.htmlColor "#000033") # set UI.textFont "15px sans-serif"
            initanzeige anzeige
            ddd <- UI.div #+ [row [column cc,column [row a,row [element canvas,element anzeige]]],element reset,element play] # set style [("margin-left","20%")]
            ddd2 <- UI.div #+ [row [column cc_left,column [row a_left,row [element field_left]],column [element sp], column cc_right, column [row a_right, row [element field_right]]]]
            body <- getBody w
            getBody w #+ [element title,element ddd]
            
            on UI.mousedown canvas $ \xy ->do
                                  let zz = convert xy
                                  liftIO $ writeIORef coords [zz]
                                  coo <- liftIO . readIORef $ coords
                                  element st # set text "ha"

            on UI.mouseup canvas $ \xy ->do
                                  pp <- liftIO . readIORef $ coords
                                  let conv = convert xy
                                  let zzz =  Data.List.sort ([conv]++pp)
                                  liftIO $ writeIORef coords zzz
                                  liftIO $ signalQSem semaphore   -- client Thread wecken
                                  element st # set text (show pp)
                                  liftIO . waitQSem $ semaphore2  -- blocken bis client Prüfung abgeschlossen hat
                                  playerflotte <- liftIO . readIORef $ field
                                  let eee = tail playerflotte
                                  let lstxxx = anzeigeCalcIndezes eee [1,2,3,4]
                                  drawanzeige anzeige lstxxx
                                  drawships canvas eee
            
            on UI.mousemove canvas $ \xy -> do
                                  let ccc = convert xy
                                  let cccc = (fromIntegral (2 + 50*((fst ccc) - 1)),fromIntegral (2 + 30*((snd ccc) - 1)))
                                  set' UI.fillStyle (UI.htmlColor "#000033") canvas
                                  drawcanvas canvas
                                  set' UI.fillStyle (UI.htmlColor "#660000") canvas
                                  UI.fillRect cccc 48 28 canvas
                                  playerflotte <- liftIO . readIORef $ field
                                  if (tail playerflotte) == [] then UI.fillRect cccc 48 28 canvas
                                  else 
                                    do
                                    let eee = tail playerflotte
                                    drawships canvas eee


            on UI.click reset $ const $ do
                                 liftIO $ writeIORef field [[(0,0)]]
                                 drawcanvas canvas
                                 initanzeige anzeige

            on UI.click play $ const $ do
                                 lst <- liftIO $ readIORef field
                                 if (length $ tail lst) /= 10 then return ()
                                 else do
                                      drawcanvas field_left
                                      drawcanvas field_right
                                      playerflotte <- liftIO . readIORef $ field
                                      let eee = tail playerflotte
                                      drawships field_left eee
                                      liftIO $ writeIORef playclicked True  -- playclicket true setzen
                                      getBody w #+ [element ddd2]           -- neue Gui Body hinzufügen
                                      liftIO $ signalQSem semaphore
                                      Graphics.UI.Threepenny.Core.delete ddd;

            on UI.mousemove field_right $ \xy -> do
                                                   xyz <- liftIO $ readIORef allowed2fire
                                                   case xyz of
                                                        True -> do
                                                                  let ccc = convert xy
                                                                  let cccc = (fromIntegral (2 + 50*((fst ccc) - 1)),fromIntegral (2 + 30*((snd ccc) - 1)))
                                                                  field_right # UI.clearCanvas
                                                                  drawcanvas field_right
                                                                  nieten <- liftIO $ readIORef nieten_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [nieten], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#660000") field_right
                                                                  rer jaja
                                                                  treffer <- liftIO $ readIORef treffer_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [treffer], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#ffff00") field_right
                                                                  rer jaja
                                                                  versenkt <- liftIO $ readIORef versenkt_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [versenkt], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#ff0000") field_right
                                                                  rer jaja
                                                                  set' UI.fillStyle (UI.htmlColor "#660000") field_right
                                                                  UI.fillRect cccc 48 28 field_right
                                                                  UI.fillRect cccc 48 28 field_right
                                                        False -> return ()

            on UI.mousedown field_right $ \xy -> do
                                                   xyz <- liftIO $ readIORef allowed2fire
                                                   case xyz of
                                                        False -> return ()
                                                        True -> do
                                                                  let ccc = convert xy
                                                                  let cccc = (fromIntegral (2 + 50*((fst ccc) - 1)),fromIntegral (2 + 30*((snd ccc) - 1)))
                                                                  liftIO $ writeIORef coords [ccc]
                                                                  liftIO.signalQSem $ semaphore
                                                                  liftIO.waitQSem $ semaphore2
                                                                  nieten <- liftIO $ readIORef nieten_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [nieten], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#660000") field_right
                                                                  rer jaja
                                                                  treffer <- liftIO $ readIORef treffer_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [treffer], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#ffff00") field_right
                                                                  rer jaja
                                                                  versenkt <- liftIO $ readIORef versenkt_right
                                                                  let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_right | w <- [versenkt], v <- w]
                                                                  set' UI.fillStyle (UI.htmlColor "#ff0000") field_right
                                                                  rer jaja


            timer <- UI.timer # set UI.interval (bpm2ms defaultBpm)
            eBeat <- accumE (0::Int) $
                  (\beat -> (beat + 1) `mod` (beats * bars)) <$ UI.tick timer
            void . onEvent eBeat $ \beat -> do
                                               bool <- liftIO $ readIORef bool_left
                                               case bool of
                                                  True -> do
                                                             liftIO $ putStrLn "Ok"
                                                             nieten <- liftIO $ readIORef nieten_left
                                                             let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_left | w <- [nieten], v <- w]
                                                             set' UI.fillStyle (UI.htmlColor "#660000") field_left
                                                             rer jaja
                                                             treffer <- liftIO $ readIORef treffer_left
                                                             let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_left | w <- [treffer], v <- w]
                                                             set' UI.fillStyle (UI.htmlColor "#ffff00") field_left
                                                             rer jaja
                                                             versenkt <- liftIO $ readIORef versenkt_left
                                                             let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 field_left | w <- [versenkt], v <- w]
                                                             set' UI.fillStyle (UI.htmlColor "#ff0000") field_left
                                                             rer jaja
                                                             liftIO $ writeIORef bool_left False
                                                  False -> return ()
            UI.start timer     -- Timer starten

drawships :: UI.Canvas -> [[(Int,Int)]] -> UI ()
drawships canvas eee = do
                         if eee == [] then return ()
                         else do
                                let jaja = [UI.fillRect (fromIntegral (2 + 50*((fst v)-1)),fromIntegral (2 + 30*((snd v) -1 ))) 48 28 canvas | w <- eee, v <- w]
                                set' UI.fillStyle (UI.htmlColor "#00ff00") canvas
                                rer jaja


drawcanvas :: UI.Canvas -> UI ()
drawcanvas canvas = do set' UI.fillStyle (UI.htmlColor "#000033") canvas
                       let hh = [UI.fillRect (x,y) 48 28 canvas | x <- [2,52..452], y <- [2,32..272]]
                       rer hh

initanzeige :: UI.Canvas -> UI ()
initanzeige anzeige = do
                       anzeige # UI.clearCanvas
                       let hh5 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..108], y <- [10]]
                       let hh4 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..81], y <- [35]]
                       let hh3 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..54], y <- [60]]
                       let hh2 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39], y <- [85]]
                       rer hh5; rer hh4; rer hh3; rer hh2
                       anzeige # UI.fillText "x 1" (155,25)
                       anzeige # UI.fillText "x 2" (155,50)
                       anzeige # UI.fillText "x 3" (155,75)
                       anzeige # UI.fillText "x 4" (155,100)

drawanzeige :: UI.Canvas -> [Int] -> UI ()
drawanzeige anzeige lstxxx = do
                               anzeige # UI.clearCanvas
                               let hh5 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..108], y <- [10]]
                               let hh4 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..81], y <- [35]]
                               let hh3 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39..54], y <- [60]]
                               let hh2 = [UI.fillRect (x,y) 25 20 anzeige | x <- [12,39], y <- [85]]
                               rer hh5; rer hh4; rer hh3; rer hh2
                               anzeige # UI.fillText ("x " ++ (show (lstxxx !! 0))) (155,25)
                               anzeige # UI.fillText ("x " ++ (show (lstxxx !! 1))) (155,50)
                               anzeige # UI.fillText ("x " ++ (show (lstxxx !! 2))) (155,75)
                               anzeige # UI.fillText ("x " ++ show (lstxxx !! 3)) (155,100)

anzeigeCalcIndezes :: [[(Int,Int)]] -> [Int] -> [Int]
anzeigeCalcIndezes field [a,b,c,d] = anzeigeCalcIndezes' xss [a,b,c,d]
                              where xxx = sortOn Data.Ord.Down (length <$> field)
                                    xss = [length $ Prelude.filter (==i) xxx | i<-[5,4,3,2]]
                                    anzeigeCalcIndezes' :: [Int] -> [Int] -> [Int]
                                    anzeigeCalcIndezes' [a5,a4,a3,a2] [b5,b4,b3,b2] = [b5-a5 , b4-a4 , b3-a3 , b2-a2]

convert :: (Int,Int) -> (Int,Int)
convert tup = (a,b)
                where a = calcx . getx $ tup
                      b = calcy . gety $ tup

getx :: (Int,Int) -> Int
getx (a,b) = a

gety :: (Int,Int) -> Int
gety (a,b) = b

calcx :: Int -> Int
calcx i = 1 + result
          where result = i `div` 50

calcy :: Int -> Int
calcy i = 1 + result
          where result = i `div` 30

rer :: [UI ()] -> UI ()
rer [tt] = tt
rer (tt:xs) = do tt; rer xs