module Gaussian (gaussian, CustomAns(..), force, generateRandom) where

import Data.List (elemIndices)
import System.Random (randoms, newStdGen)
import GHC.Conc ( pseq )
import GHC.Conc.Sync (par)
import Data.List (sortBy)
import Data.Ord (comparing)


type Row = [Float]
type Matrix = [Row]
type Vector = [Float]
type Point = (Int, Int)


negativeInfinity :: Float
negativeInfinity = -100000000000

eps :: Float
eps = 0.00000001

subtractRows :: Row -> Row -> Row
subtractRows [] _ = []
subtractRows _ [] = []
subtractRows (x:xs) (y:ys) = (x - y) : subtractRows xs ys


multiplyRowByNumber :: Row -> Float -> Row
multiplyRowByNumber [] _ = []
multiplyRowByNumber (x:xs) value = x*value : multiplyRowByNumber xs value


calculateScalingFactor :: Int -> Row -> Row -> Float
calculateScalingFactor index x y
    | index >= 0 && (index < length x) && (index < length y) = x !! index / y !! index
    | otherwise                                              = 0

-- last element is value from vector, we should ignore
customMax :: Row -> Float
customMax [] = negativeInfinity
customMax [_] = negativeInfinity
customMax (x:xs)
    | x > val   = x
    | otherwise = val
    where val = customMax xs


maxAtIndex :: Matrix -> [Int] -> Float -> Int -> Point -> Point
maxAtIndex [] _ _ _ index = index
maxAtIndex (x:xs) usedRows value rowId index
    | elem rowId usedRows || value >= customMax x = maxAtIndex xs usedRows value (rowId + 1) index
    | value < customMax x && customMax x /= 0     = maxAtIndex xs usedRows (customMax x) (rowId + 1) (rowId, newIndex)
    | otherwise                                   = (-1, -1)
    where
        newIndex = head $ elemIndices (customMax x) x


force :: [a] -> ()
force [] = ()
force (x:xs) = x `pseq` force xs


goAlongRows :: Matrix -> Int -> Row -> Point -> Bool -> Matrix
goAlongRows [] _ _ _ _ = []
goAlongRows (mat:mats) rowInd rowToSub maxIndex@(maxRowIndex, maxColIndex) paral
    | paral && maxRowIndex == rowInd = nextRowsResult `par` (force divOnOwn `pseq` (divOnOwn:nextRowsResult))
    | paral             = nextRowsResult `par` ( force subResult  `pseq` (subResult:nextRowsResult))
    | maxRowIndex == rowInd = divOnOwn:nextRowsResult
    | otherwise           = subResult:nextRowsResult
    where
        subResult = if checkLessZero res
                    then multiplyRowByNumber res (-1)
                    else res
        divOnOwn = multiplyRowByNumber mat (1/(mat !! maxColIndex))
        nextRowsResult = goAlongRows mats (rowInd + 1) rowToSub maxIndex paral
        res = subtractRows mat (multiplyRowByNumber rowToSub (calculateScalingFactor maxColIndex mat rowToSub))
        checkLessZero :: Row -> Bool
        checkLessZero [] = True
        checkLessZero [_] = True
        checkLessZero (xRow:xsRow)
            | xRow > 0  = False
            | otherwise = checkLessZero xsRow


addVectorToMartix :: Matrix -> Vector -> Matrix
addVectorToMartix [] [] = []
addVectorToMartix [] _  = []
addVectorToMartix _ []  = []
addVectorToMartix (mat:mats) (vec:vecs) = (mat ++ [vec]) : addVectorToMartix mats vecs


findMainPivot :: Matrix -> [Int] -> Bool -> Matrix
findMainPivot mat usedRows paral
    | row == -1 || col == -1        = mat
    | length usedRows /= length mat = findMainPivot (goAlongRows mat 0 rowToSub maxIndex paral) (row:usedRows) paral
    | otherwise                     = mat
    where
        maxIndex@(row, col) = maxAtIndex mat usedRows negativeInfinity 0 (-1, -1)
        rowToSub = mat !! row


data CustomAns = Exists Vector
                | NegativeInfinity
                | NotExists
    deriving (Show, Eq)


firstNonZeroIndex :: Row -> Int
firstNonZeroIndex row = length $ takeWhile (== 0) row


sortRows :: Matrix -> Matrix
sortRows mat = sortBy (comparing firstNonZeroIndex) mat


gaussian :: Matrix -> Vector -> Bool -> CustomAns
gaussian [] _ _ = NotExists
gaussian mat vec paral = 
    let sortedMatrix = sortRows $ findMainPivot (addVectorToMartix mat vec) [] paral
    in checkResult sortedMatrix


checkRowResult :: Row -> Int
checkRowResult (x:xs)
    | null xs && abs x < eps = -1
    | null xs                = 0
    | abs x < eps            = checkRowResult xs
    | otherwise              = 1


checkResult :: Matrix -> CustomAns
checkResult []          = Exists []
checkResult (mat:mats)
    | ans == NotExists          = NotExists
    | ans == NegativeInfinity   = NegativeInfinity
    | curCheck == 0             = NotExists
    | curCheck == -1            = NegativeInfinity
    | otherwise                 = Exists value
    where
        curCheck = checkRowResult mat
        ans = checkResult mats
        value = case ans of
            Exists a -> last mat:a
            _ -> []


generateMatrix :: Int -> Int -> IO Matrix
generateMatrix 0 _ = return []
generateMatrix n m = do
    g <- newStdGen
    let row = take m (randoms g :: [Float])
    ans <- generateMatrix (n-1) m
    return (row:ans)


generateRandom :: Int -> IO (Matrix, Vector)
generateRandom n = do
    mat <- generateMatrix n n
    g <- newStdGen
    let vec = take n (randoms g :: [Float])
    return (mat, vec)