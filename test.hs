import Nonominos

m1 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
    (PointValue 0 3 1)  (Point 0 4) (Point 1 1)
    (Point 1 2)         (Point 1 3) (Point 2 2)
    "\x1b[41;30m"  --Red
m2 = Points (PointValue 0 0 2)  (Point 0 1)         (PointValue 0 2 3)
    (Point 0 3)         (Point 1 (-1))      (Point 1 0)
    (Point 1 1)         (PointValue 1 2 9)  (Point 1 3)
    "\x1b[42;30m"  --Green
m3 = Points (PointValue 0 0 6)  (Point 1 0)         (Point 1 1)
    (Point 1 3)         (PointValue 2 0 8)  (Point 2 1)
    (Point 2 2)         (Point 2 3)         (PointValue 3 2 4)
    "\x1b[43;30m"  --Yellow
m4 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
    (PointValue 0 3 8)  (Point 0 4) (Point 1 3)
    (Point 1 4)         (Point 2 3) (Point 2 4)
    "\x1b[44;30m"  --Blue
m5 = Points (PointValue 0 0 7)      (PointValue 1 (-1) 5)   (Point 1 0)
    (PointValue 2 (-2) 2)   (Point 2 (-1))          (PointValue 3 (-2) 9)
    (PointValue 4 (-2) 1)   (Point 5 (-3))          (Point 5 (-2))
    "\x1b[45;30m"  --Magenta
m6 = Points (PointValue 0 0 4)  (Point 0 1)    (PointValue 1 0 6)
    (Point 1 1)         (Point 2 (-1)) (Point 2 0)
    (Point 2 1)         (Point 3 1)    (Point 4 1)
    "\x1b[46;30m"  --Cyan
m7 = Points (Point 0 0)         (Point 0 1)         (Point 1 0)
    (Point 1 1)         (PointValue 2 0 3)  (Point 2 1)
    (PointValue 3 0 4)  (Point 3 1)         (Point 4 0)
    "\x1b[47;30m"  --White
m8 = Points (Point 0 0)     (Point 0 1)         (Point 1 0)
    (Point 1 1)     (Point 2 0)         (Point 2 1)
    (Point 3 (-1))  (PointValue 3 0 7)  (Point 3 1)
    "\x1b[100;30m"  --Light Gray
m9 = Points (PointValue 0 0 8)  (Point 0 1)         (PointValue 0 2 5)
    (PointValue 1 0 7)  (Point 1 1)         (PointValue 1 2 9)
    (PointValue 2 0 3)  (PointValue 2 1 6)  (Point 2 2)
    "\x1b[40;37m"  --Default


-- m1 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
--             (Point 0 3 )  (Point 0 4) (Point 1 1)
--             (Point 1 2)         (Point 1 3) (Point 2 2)
--             "\x1b[41;30m"  --Red
-- m2 = Points (Point 0 0 )  (Point 0 1)         (Point 0 2 )
--             (Point 0 3)         (Point 1 (-1))      (Point 1 0)
--             (Point 1 1)         (Point 1 2 )  (Point 1 3)
--             "\x1b[42;30m"  --Green
-- m3 = Points (Point 0 0 )  (Point 1 0)         (Point 1 1)
--             (Point 1 3)         (Point 2 0)  (Point 2 1)
--             (Point 2 2)         (Point 2 3)         (Point 3 2)
--             "\x1b[43;30m"  --Yellow
-- m4 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
--             (Point 0 3 )  (Point 0 4) (Point 1 3)
--             (Point 1 4)         (Point 2 3) (Point 2 4)
--             "\x1b[44;30m"  --Blue
-- m5 = Points (Point 0 0)      (Point 1 (-1) )   (Point 1 0)
--             (Point 2 (-2) )   (Point 2 (-1))          (Point 3 (-2) )
--             (Point 4 (-2) )   (Point 5 (-3))          (Point 5 (-2))
--             "\x1b[45;30m"  --Magenta
-- m6 = Points (Point 0 0 )  (Point 0 1)    (Point 1 0 )
--             (Point 1 1)         (Point 2 (-1)) (Point 2 0)
--             (Point 2 1)         (Point 3 1)    (Point 4 1)
--             "\x1b[46;30m"  --Cyan
-- m7 = Points (Point 0 0)         (Point 0 1)         (Point 1 0)
--             (Point 1 1)         (Point 2 0)  (Point 2 1)
--             (Point 3 0 )  (Point 3 1)         (Point 4 0)
--             "\x1b[47;30m"  --White
-- m8 = Points (Point 0 0)     (Point 0 1)         (Point 1 0)
--             (Point 1 1)     (Point 2 0)         (Point 2 1)
--             (Point 3 (-1))  (Point 3 0 )  (Point 3 1)
--             "\x1b[100;30m"  --Light Gray
-- m9 = Points (Point 0 0 )  (Point 0 1)         (Point 0 2 )
--             (Point 1 0 )  (Point 1 1)         (Point 1 2 )
--             (Point 2 0 )  (Point 2 1 )  (Point 2 2)
--             "\x1b[40;37m"  --Default

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

-- table initialMatrix nonominoPositioned
-- table (head solved) nonominoPositioned
solved1 = solvePossibles initialMatrix initialMatrixPossible nonominoPositioned
-- debug :: Bool
-- debug = canSetAllNonominos matrix m
