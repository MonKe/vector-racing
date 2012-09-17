module Main where

import Box (Box(..),CIndex,display,fillD,lineC,lineD,pin)
import qualified Data as D

data Car     = Car Vector Vector Int String String Box -- position speed lap checkpoints msg (+underground) aspect
type Vector  = (Int,Int) -- (x,y)

main :: IO ()
main = do putStr "\ESC[2J"
          turn c (start c 2) (start c 2) 1
     where c = D.circuits !! 0

turn :: D.Circuit -> [Car] -> [Car] -> Int -> IO ()
turn c tc [] l = turn c tc tc l
turn (D.Circuit t stp cp ci m) tcrs ((Car (x,y) (sx,sy) l ccp msg b):crs) laps = do
     display ci $ info $ pin (20,1) (nb tcrs m) (fillD (61,14) '*' ' ')
     if l == laps then return ()
                  else do key <- getLine
                          turn (D.Circuit t stp cp ci m) ((take (player-1) tcrs) ++ (move $ from key)) (tail $ move $ from key) laps
     where from "\ESC[A" = (0,-1)
           from "\ESC[B" = (0,1)
           from "\ESC[C" = (1,0)
           from "\ESC[D" = (-1,0)
           from a        = (0,0)
           nb [Car (x,y) _ _ _ _ b] m = pin (x,y) b m
           nb ((Car (x,y) _ _ _ _ b):aa) m = pin (x,y) b (nb aa m)
           player = (length tcrs) - (length crs)
           info a = pin (1,1) (lineD '*' t)
                  $ pin (1,2) (lineD '*' $ "lap:" ++ show l ++ "/" ++ show laps)
                  $ pin (1,3) (lineD '*' $ "checks:" ++ ccp)
                  $ pin (1,5) (lineC '*' pl $ take (length pl) $ repeat $ head $ show player)
                  $ pin (1,6) (lineD '*' msg) a
                  where pl = "Player " ++ show player
           move (kx,ky) = (Car (gx,gy) ns nl ncp nms ncar : crs)
              where tile = ((r m) !! gy) !! gx
                    area | (sx,sy) == (0,0) = [[tile]]
                         | True = map (\a -> range x gx a) $ range y gy $ r m
                         where range a b c = take ((max a b)-(min a b)+1) $ drop (min a b) c
                               min a b = if a <= b then a else b
                               max a b = if a <= b then b else a
                    checkpoint = filter (\a -> elem a cp) $ concat area
                    ncar | nl == laps = num (show player) D.win
                         | tile == '#' = D.crash
                         | tile == ' ' = D.crashU
                         | True = num (show player) D.car
                    num a b = pin (0,0)  (lineC '*' a a) b
                    nms | nl == laps  = D.message !! 1
                        | tile == '#' = D.message !! 0
                        | True = " "
                    ncp | null ccp = cp
                        | (not $ null checkpoint) && head ccp == head checkpoint = tail ccp
                        | True = ccp
                    nl = if (not $ null checkpoint) && (not $ null ccp) && head ccp == head checkpoint && length ccp == 1 then l+1 else l
                    ns | elem tile ['#',' '] = (0,0)
                       | True = (sx+kx,sy+ky)
                    gx = limit (x+sx+kx) $ length $ head $ r m
                    gy = limit (y+sy+ky) $ length $ r m
                    limit a b | a > 0 && a < b = a | a >= b = b-1 | True = 0
                    r (CBox _ (_,a)) = a

start :: D.Circuit -> Int -> [Car]
start (D.Circuit _ stp cp _ _) 1   = [Car (head stp) (0,0) 0 cp (D.message !! 2) (pin (0,0) (lineC '*' "1" "1") D.car)]
start c@(D.Circuit _ stp cp _ _) i = start c (i-1) ++ [Car (stp !! (i-1)) (0,0) 0 cp (D.message !! 2) (pin (0,0) (lineC '*' (show $ i) (show $ i)) D.car)]
