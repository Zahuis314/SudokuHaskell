module Nonominos
where

import Data.List

data Point = Point Int Int |
             PointValue Int Int Int
             deriving (Show)
data Nonomino = Points { p0 :: Point
                       , p1 :: Point
                       , p2 :: Point
                       , p3 :: Point
                       , p4 :: Point
                       , p5 :: Point
                       , p6 :: Point
                       , p7 :: Point
                       , p8 :: Point
                       , color :: String
                       } deriving (Show)

value (Point _ _) = 0
value (PointValue _ _ v) = v
x (Point x _) = x
x (PointValue x _ _) = x
y (Point _ y) = y
y (PointValue _ y _) = y



--Imprimir una matriz
print_ sudoku nms (x,y) =  putStr $ col ++ " " ++ show ((sudoku !! x) !! y) ++ " " ++ "\x1b[49;39m"
                            where
                            col = color nm
                            nm = getNonomino nms x y
table :: [[Int]] -> [Nonomino] -> IO()
table sudoku nms = mapM_ printRow (map (\x -> [(x,y)| y<- [0..8]]) [0..8])
    where
    printRow xs = mapM_ (print_ sudoku nms) xs >> putStrLn ""
    
    -- print_ x =  putStr $ show x ++ "\t" 
    -- | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
    -- | otherwise = mapM_ printRow xxs 
            -- xxs = map (\x -> [(x,y)| y<- [0..8]]) [0..8]

-- tableCell :: [[Int]] -> [Nonomino] -> Int -> Int -> String
-- tableCell sudoku pieces x y = putStr show (sudoku !! x !! y)

-- myTable sudoku pieces = [tableCell sudoku pieces x y | x <- [0..8], y <- [0..8]] >> putStr ""

--Obtener todos los puntos de un Nonomino--
getPoints :: Nonomino -> [Point]
getPoints (Points p0 p1 p2 p3 p4 p5 p6 p7 p8 _) = [p0,p1,p2,p3,p4,p5,p6,p7,p8]

--Obtener las coordenadas de un punto--
getCoordenate :: Point -> [Int]
getCoordenate (Point x y) = [x,y]
getCoordenate (PointValue x y _) = [x,y]

--Reemplazar en una lista el elemento i-esimo--
replace :: [a] -> Int -> a -> [a]
replace list i element = take i list ++ [element] ++ drop (i + 1) list

--Reemplazar en una matriz el elemento i,j-esimo--
replaceMatrix :: [[Int]] -> Point -> [[Int]]
replaceMatrix list (Point i j) = replace list i replacedRow
                               where
                               row = list !! i
                               replacedRow = replace row j 0
replaceMatrix list (PointValue i j v) = replace list i replacedRow
                               where
                               row = list !! i
                               replacedRow = replace row j v


curriedReplaceMatrix :: [[Int]] -> Point -> [[Int]]
curriedReplaceMatrix = replaceMatrix

--Trasladar un punto--
traslatePoint :: Point -> Int -> Int -> Point
traslatePoint (Point x y) i j = Point (x+i) (y+j)
traslatePoint (PointValue x y v) i j = PointValue (x+i) (y+j) v

--Trasladar un nonomino--
traslateNonomino :: Nonomino -> Int -> Int -> Nonomino
traslateNonomino (Points p1 p2 p3 p4 p5 p6 p7 p8 p9 color) i j = Points mp1 mp2 mp3 mp4 mp5 mp6 mp7 mp8 mp9 color
                                                   where
                                                   mp1 = traslatePoint p1 i j
                                                   mp2 = traslatePoint p2 i j
                                                   mp3 = traslatePoint p3 i j
                                                   mp4 = traslatePoint p4 i j
                                                   mp5 = traslatePoint p5 i j
                                                   mp6 = traslatePoint p6 i j
                                                   mp7 = traslatePoint p7 i j
                                                   mp8 = traslatePoint p8 i j
                                                   mp9 = traslatePoint p9 i j

--Setear en una matriz un nonomino--
setNonomino :: [[Int]] -> Nonomino -> Int -> Int -> [[Int]]
setNonomino list nm i j = foldl replaceMatrix list points
                    where
                    points = getPoints (traslateNonomino nm i j)

                    --Busca la primera casilla libre--
findFirstFree :: [[Int]] -> (Int,Int)
findFirstFree sudoku  = head [
    (x,y) |
    (t,x) <- zip sudoku [0..],
    (e,y) <- zip t [0..],
    e == (-1)
    ]

valid :: Int -> Int -> Bool
valid x y = x >= 0 && y >= 0 && x < 9 && y < 9

--Retorna si se puede poner un nonomino en la matrix en i,j--
canSetNonomino :: [[Int]] -> Nonomino -> Int -> Int -> Bool
canSetNonomino matrix nm i j = all (\[x,y] -> valid x y && (((matrix !! x) !! y) == (-1)) ) coordPoint
                                where
                                points = getPoints (traslateNonomino nm i j)
                                coordPoint = map getCoordenate points

setMatrixNonomino matrix x = setNonomino matrix x i j
                            where
                            free = findFirstFree matrix
                            i = fst free
                            j = snd free

setAllNonominos :: [[Int]] -> [Nonomino] -> [Nonomino]
setAllNonominos matrix [] = []
setAllNonominos matrix (x:xs) = nm : setAllNonominos cMatrix xs
                                where
                                nm = traslateNonomino x i j
                                free = findFirstFree matrix
                                i = fst free
                                j = snd free
                                cMatrix = setNonomino matrix x i j

getSettedMatrix :: [[Int]] -> [Nonomino] -> [[Int]]
getSettedMatrix matrix [] = matrix
getSettedMatrix matrix (x:xs) = getSettedMatrix cMatrix xs
                                where
                                    cMatrix = setMatrixNonomino matrix x

canSetAllNonominos :: [[Int]] -> [Nonomino] -> Bool
canSetAllNonominos matrix [] = checkSudoku matrix
canSetAllNonominos matrix (x:xs)
                                | canSetNonomino matrix x i j = canSetAllNonominos cMatrix xs
                                | otherwise = False
                                where
                                    free = findFirstFree matrix
                                    i = fst free
                                    j = snd free
                                    cMatrix = setNonomino matrix x i j

checkRow :: [[Int]] -> Bool
checkRow [] = True
checkRow (x:xs) = (length values == length uniqValues) && checkRow xs
                    where
                    values = filter (>0) x
                    uniqValues = nub values

checkColumn :: [[Int]] -> Bool
checkColumn matrix = checkRow (transpose matrix)

checkSudoku :: [[Int]] -> Bool
checkSudoku matrix = checkRow matrix && checkColumn matrix

solveNonominosPosition :: [[Int]] -> [Nonomino] -> [[Nonomino]]
solveNonominosPosition matrix nms = filter (canSetAllNonominos matrix) nmPermutations
                                    where nmPermutations = permutations nms
---------------------------------------------------
oposite :: [Int] -> [Int]
oposite a = [1,2,3,4,5,6,7,8,9] \\ a

getRow :: [[Int]] -> Int -> [Int]
getRow sudoku x = oposite (filter (>0) (sudoku !! x))

getColumn :: [[Int]] -> Int -> [Int]
getColumn [] y = []
getColumn (x:xs) y  | (x!!y) > 0 = (x!!y):getColumn xs y
                    | otherwise = getColumn xs y

hasPoint :: [Point] -> Int -> Int -> Bool
hasPoint [] _ _ = False
hasPoint (Point x y : pts) i j | x == i && y == j = True
                             | otherwise = hasPoint pts i j

hasPoint (PointValue x y _ : pts) i j | x == i && y == j = True
                             | otherwise = hasPoint pts i j
                             
getNonomino :: [Nonomino] -> Int -> Int -> Nonomino --Maybe Nonomino
-- getNonomino [] _ _ = Nothing
getNonomino (xx:xs) x y | hasPoint (getPoints xx) x y = xx--Just xx
                        | otherwise = getNonomino xs x y

getPiece :: [[Int]] -> [Nonomino] -> Int -> Int -> [Int]
getPiece _ [] _ _ = []
getPiece sudoku nms x y = oposite values
                            where
                             nm = getNonomino nms x y
                             points = getPoints nm
                             values = filter (>0) (map value points)
---------------------------------------------------
calcPossible :: [[Int]] -> [Nonomino] -> (Int,Int) -> [Int]
calcPossible sudoku nms (x,y) | sudoku !! x !! y > 0 = []
                              | otherwise = getRow sudoku x `intersect` oposite (getColumn sudoku y) `intersect` getPiece sudoku nms x y

calcRowPossible :: [[Int]] -> [Nonomino] -> Int -> [[Int]]
calcRowPossible sudoku nms x = [ calcPossible sudoku nms (x,y) | y <- [0..8]]

calcMatrixPossible :: [[Int]] -> [Nonomino] -> [[[Int]]]
calcMatrixPossible sudoku nms = [ calcRowPossible sudoku nms x | x <- [0..8]]

---------------------------------------------------
removePossibleRow :: [[[Int]]] -> Int -> Int -> [[[Int]]]
removePossibleRow possibles x value = take x possibles ++ [element] ++ drop (x + 1) possibles
                            where
                            row = possibles !! x 
                            element = map (\y -> y \\ [value]) row

removePossibleColumn :: [[[Int]]] -> Int -> Int -> [[[Int]]]
removePossibleColumn possibles y value = transpose (removePossibleRow (transpose possibles) y value)

removePossiblePieceCoord :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
removePossiblePieceCoord possibles x y value = take x possibles ++ [changedRow] ++ drop (x + 1) possibles
                                            where
                                            cell = row !! y
                                            changedCell = cell \\ [value]
                                            row = possibles !! x
                                            changedRow = take y row ++ [changedCell] ++ drop (y + 1) row

removePossiblePiece :: [[[Int]]] -> [Nonomino] -> Int -> Int -> Int -> [[[Int]]]
removePossiblePiece possibles nms x y value = foldl (\m [i,j] -> removePossiblePieceCoord m i j value) possibles coordPoint
                                            where
                                                nm = getNonomino nms x y
                                                points = getPoints nm
                                                coordPoint = map getCoordenate points

removePossible :: [[[Int]]] -> [Nonomino] -> Int -> Int -> Int -> [[[Int]]]
removePossible possibles nms x y value = removedAll
                                            where
                                            removedRow = removePossibleRow possibles x value
                                            removedCol = removePossibleColumn removedRow y value
                                            removedAll = removePossiblePiece removedCol nms x y value
---------------------------------------------------
solve :: [[Int]] -> [[[Int]]] -> [Nonomino] -> Int -> Int ->  [[[Int]]]
solve matrix _ _ 9 _ = [matrix]
solve matrix possibles nms i 9 = solve matrix possibles nms (i+1) 0
solve matrix possibles nms i j | cell > 0 = solve matrix possibles nms i (j+1)
                               | otherwise = concatMap (\(nMatrix,nPossibles) -> solve nMatrix nPossibles nms i (j+1)) comb
                                where
                                    cell = (matrix !! i) !! j
                                    possible = (possibles !! i) !! j
                                    -- element = possible
                                    row = matrix !! i
                                    newMatrix = [take i matrix ++ [take j row ++ [e] ++ drop (j + 1) row] ++ drop (i + 1) matrix | e <- possible]
                                    newPossibles = [removePossible possibles nms i j p | p <- possible]
                                    comb = zip newMatrix newPossibles
                                    -- newRow = take j row ++ [element] ++ drop (j + 1) row
                                    -- newMatrix = take i row ++ [newRow] ++ drop (i + 1) row
                                    -- newPossibles = removePossible possibles nms i j element
---------------------------------------------------
myReplicator :: Int -> [[Int]]
myReplicator n = replicate 9 (replicate 9 n)
matrix = myReplicator (-1)