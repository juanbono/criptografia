-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
module Main where
import Criterion.Main

main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
