import Test.HUnit
import Test.QuickCheck

import Board qualified as B
import Minimax qualified as M

main :: IO ()
main = do 
    putStrLn "*** Testing Board ***"
    B.test_all
    B.qc
    putStrLn "*** Testing Minimax ***"
    M.qc
