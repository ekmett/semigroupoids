module Main where

import Build_doctests (deps)
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>=
       doctest . ((["-Wall", "-fno-warn-warnings-deprecations"]
                   ++ map ("-package="++) deps) ++)
