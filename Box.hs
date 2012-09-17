module Box where
data Box    = DBox  Char Matrix -- alpha draw
            | CBox  Char (Matrix,Matrix) -- alpha (draw,color)
            | IOBox Char Bool (Matrix,Matrix,Matrix)  -- alpha stretch (draw,color,input)
            deriving (Show,Eq)
type CIndex = [(Char,[Int])]
type Matrix = [[Char]]
getDM :: Box -> Matrix
getDM (DBox _ m) = m
toC :: Box -> Box
toC (DBox z (d:dd)) = CBox z ((d:dd),c)
   where c = take (length (d:dd)) $ repeat $ take (length d) $ repeat z
-- base box creation functions
-- 1-line box
lineD :: Char -> String -> Box -- alpha draw-string
lineD _ [] = error $ err !! 1
lineD z s  = DBox z [s]
lineC :: Char -> String -> String -> Box -- alpha draw-string
lineC _ [] c = error $ err !! 1
lineC z d c  = CBox z ([d],[c])
-- empty box
fillD :: (Int,Int) -> Char -> Char -> Box
fillD (w,h) z d = if w*h > 0 then DBox z $ m h d else error $ err !! 0
   where m a b = if a > 0 then [take w $ repeat b] ++ m (a-1) b else []
fillC :: (Int,Int) -> Char -> Char -> Char -> Box
fillC (w,h) z d c = CBox z (getDM $ fillD (w,h) z d,getDM $ fillD (w,h) z c)
-- pin functions
pin :: (Int,Int) -> Box -> Box -> Box -- (width, height) alpha inbox outbox
pin (x,y) (DBox z i) (DBox _ o) = DBox z (r i o)
    where r a b  = if y > 0 then take y b ++ m a (drop y b) else m (drop y a) b
          m _ [] = []
          m [] a = a
          m (a:aa) (b:bb) = [take x b ++ ps a b ++ drop (x + length a) b] ++ m aa bb
          ps a b = zipWith (\a b -> pt a b) a (take (length a) (drop x b))
          pt a b = if a == z then b else a
pin p (DBox z i) o = pin p (toC $ DBox z i) o
pin p i (DBox z o) = pin p i (toC $ DBox z o)
pin p (CBox z (i,ic)) (CBox x (o,oc)) = CBox z (getDM $ pin p (DBox z i) (DBox x o),getDM $ pin p (DBox z ic) (DBox x oc))
center :: Box -> Box -> Box
center i@(CBox _ (id@(ir:_),_)) o@(CBox _ (od@(or:_),_)) = pin (c or ir,c od id) i o
    where c a b = (div (length a) 2)-(div (length b) 2)
-- other pin variants : border, pattern, stretch...
-- outputs the box as a coloured string
display :: CIndex -> Box -> IO ()
display i (CBox z (d,c)) = putStr $ "\ESC[H" ++ (r $ zip (unlines d) (unlines c))
    where r []         = []
          r ((a,b):aa) = t (if a /= z then a else ' ') b ++ r aa
          t a b = if b /= (fst $ head i)
                     then (e $ s b i) ++ [a] ++ (e $ s (fst $ head i) i)
                     else [a]
          s _ []         = [] -- s:search color-char color-map
          s a ((b,c):bb) = if a == b then c else [] ++ s a bb
          e [] = e (snd $ head i) -- e:escape color-list
          e a  = "\ESC[" ++ (init $ concat $ map (\a -> show a ++ ";") a) ++ "m"
err :: [String]
err = ["[Box.new] cannot create box with area <= 0",
       "[Box.string] cannot create box from empty string"]
colors :: CIndex
colors = [('0',[0]),                                                 -- reset color
            ('*',[1]),('_',[4]),('!',[4]),('%',[7]),
            ('x',[30]),('r',[31]),('g',[32]),('y',[33]),('b',[34]),
            ('m',[35]),('c',[36]),('w',[37]),('d',[39]),
            ('D',[40]),('R',[41]),('G',[42]),('Y',[43]),('B',[44]),
            ('M',[45]),('C',[41]),('W',[41]),('D',[41]),
            ('1',[41,30]),('2',[31,4,1]),('3',[41,30,1])]                  -- sample colors
b2 :: Box
b2 = CBox     '#'
   (["+--------+",
     "|        |",
     "+--------+"],
    ["1111111111",
     "122§§§§§§1",
     "1111113131"])
--  display colorGrid $ wrap (pin (2,1) (string '§' "Hello!") b2) $ wrap (new (20,5) '§' ':' 'y') $ new (60,15) '§' ' ' '0'
