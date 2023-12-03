import Test.HUnit
import Gaussian


testGaussian = TestCase $ do
    let mat1 = [[2.0, 1.0, -1.0], [3.0, -2.0, 4.0], [-1.0, 5.0, -3.0]]
    let vec1 = [8.0, -11.0, 22.0]
    let expected = Exists [1.2777778, 3.4722223, -1.9722221]
    assertEqual "Test for gaussian function" expected (gaussian mat1 vec1 False)


testGaussianParallel = TestCase $ do
    let mat1 = [[2.0, 1.0, -1.0], [3.0, -2.0, 4.0], [-1.0, 5.0, -3.0]]
    let vec1 = [8.0, -11.0, 22.0]
    let expected = Exists [1.2777778, 3.4722223, -1.9722221]
    assertEqual "Test for gaussian function with parallel computation" expected (gaussian mat1 vec1 True)

testGenerateRandom = TestCase $ do
    let size = 10
    (mat, vec) <- generateRandom size
    let matRows = length mat
    let matCols = if null mat then 0 else length (head mat)
    let vecSize = length vec

    assertEqual "Check number of rows in matrix" size matRows
    assertEqual "Check number of columns in matrix" size matCols
    assertEqual "Check size of vector" size vecSize


tests = TestList [TestLabel "Gaussian Test" testGaussian, TestLabel "Gaussian Parallel Test" testGaussianParallel, TestLabel "Generate Random Test" testGenerateRandom]


main :: IO ()
main = do
    runTestTT tests
    return ()