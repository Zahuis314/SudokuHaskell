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
                       } deriving (Show)

value (Point _ _) = 0
value (PointValue _ _ v) = v
x (Point x _) = x
x (PointValue x _ _) = x
y (Point _ y) = y
y (PointValue _ y _) = y



--Imprimir una matriz
print_ x =  putStr $ show x ++ "\t" 
table xxs 
    | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
    | otherwise = mapM_ printRow xxs 
        where printRow xs =  mapM_ print_ xs >> putStrLn "" 

--Obtener todos los puntos de un Nonomino--
getPoints :: Nonomino -> [Point]
getPoints (Points p0 p1 p2 p3 p4 p5 p6 p7 p8) = [p0,p1,p2,p3,p4,p5,p6,p7,p8]

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
traslateNonomino (Points p1 p2 p3 p4 p5 p6 p7 p8 p9) i j = Points mp1 mp2 mp3 mp4 mp5 mp6 mp7 mp8 mp9
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
setNonomino list nm i j = foldl curriedReplaceMatrix list points
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

allNonominosSetted :: [Bool] -> Bool
allNonominosSetted = and

setAllNonominos :: [[Int]] -> [Nonomino] -> [Nonomino]
setAllNonominos matrix [] = []
setAllNonominos matrix (xx:xs) = nm : setAllNonominos newMatrix xs
                                where
                                free = findFirstFree matrix
                                x = fst free
                                y = snd free
                                nm = traslateNonomino xx x y
                                newMatrix = setNonomino matrix xx x y
getSettedMatrix :: [[Int]] -> [Nonomino] -> [[Int]]
getSettedMatrix matrix [] = matrix
getSettedMatrix matrix (x:xs) = getSettedMatrix cMatrix xs
                                where
                                    cMatrix = setNonomino matrix x i j
                                    free = findFirstFree matrix
                                    i = fst free
                                    j = snd free

canSetAllNonominos :: [[Int]] -> [Nonomino] -> Bool
canSetAllNonominos matrix [] = checkSudoku matrix
canSetAllNonominos matrix (x:xs)
                                | canSetNonomino matrix x i j = 
                                    let cMatrix = setNonomino matrix x i j
                                    in canSetAllNonominos cMatrix xs
                                | otherwise = False
                                where
                                    free = findFirstFree matrix
                                    i = fst free
                                    j = snd free

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


m1 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
            (PointValue 0 3 1)  (Point 0 4) (Point 1 1)
            (Point 1 2)         (Point 1 3) (Point 2 2)
m2 = Points (PointValue 0 0 2)  (Point 0 1)         (PointValue 0 2 3)
            (Point 0 3)         (Point 1 (-1))      (Point 1 0)
            (Point 1 1)         (PointValue 1 2 9)  (Point 1 3)
m3 = Points (PointValue 0 0 6)  (Point 1 0)         (Point 1 1)
            (Point 1 3)         (PointValue 2 0 8)  (Point 2 1)
            (Point 2 2)         (Point 2 3)         (PointValue 3 2 4)
m4 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
            (PointValue 0 3 8)  (Point 0 4) (Point 1 3)
            (Point 1 4)         (Point 2 3) (Point 2 4)
m5 = Points (PointValue 0 0 7)      (PointValue 1 (-1) 5)   (Point 1 0)
            (PointValue 2 (-2) 2)   (Point 2 (-1))          (PointValue 3 (-2) 9)
            (PointValue 4 (-2) 1)   (Point 5 (-3))          (Point 5 (-2))
m6 = Points (PointValue 0 0 4)  (Point 0 1)    (PointValue 1 0 6)
            (Point 1 1)         (Point 2 (-1)) (Point 2 0)
            (Point 2 1)         (Point 3 1)    (Point 4 1)
m7 = Points (Point 0 0)         (Point 0 1)         (Point 1 0)
            (Point 1 1)         (PointValue 2 0 3)  (Point 2 1)
            (PointValue 3 0 4)  (Point 3 1)         (Point 4 0)
m8 = Points (Point 0 0)     (Point 0 1)         (Point 1 0)
            (Point 1 1)     (Point 2 0)         (Point 2 1)
            (Point 3 (-1))  (PointValue 3 0 7)  (Point 3 1)
m9 = Points (PointValue 0 0 8)  (Point 0 1)         (PointValue 0 2 5)
            (PointValue 1 0 7)  (Point 1 1)         (PointValue 1 2 9)
            (PointValue 2 0 3)  (PointValue 2 1 6)  (Point 2 2)

-- m66 = Points (PointValue 0 0 4)  (Point 0 1)    (PointValue 1 0 6)
--              (Point 1 1)         (Point 2 (-1)) (Point 2 0)
--              (Point 2 1)         (Point 2 2)    (Point 2 3)
-- m88 = Points (Point 0 0)  (Point 0 1)         (Point 0 2)
--              (Point 1 0)  (Point 1 1)         (Point 1 2)
--              (Point 2 0)  (PointValue 2 1 7)  (Point 2 2)

m = [m1,m2,m3,m4,m5,m6,m7,m8,m9]
-- mm1 = [m1,m2,m3,m4,m5,m6,m7,m8,m9]
-- mm2 = [m1,m2,m3,m4,m5,m66,m7,m9,m88]

perm = solveNonominosPosition matrix m
nonominoPositioned = setAllNonominos matrix (head perm)
initialMatrix = getSettedMatrix matrix (head perm)
initialMatrixPossible = calcMatrixPossible initialMatrix nonominoPositioned
solved = solve initialMatrix initialMatrixPossible nonominoPositioned 0 0
-- debug :: Bool
-- debug = canSetAllNonominos matrix m

i=5
j=7
