module Main where

import qualified Data.CycleRoll as CR
import qualified Data.CycleRoll.LCP as LCP

import System.Environment
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Heap as Heap
import qualified Data.Foldable as Foldable

interleave _ [] = []
interleave [] xs = xs
interleave (x1:xs1) (x2:[]) = [x2]
interleave (x1:xs1) (x2:xs2) = [x2,x1] ++ (interleave xs1 xs2)

main = do
  --line <- readline "
  args <- getArgs
  process $ intercalate " " args
  return ()
  where
    show_groups grps =
      "[\n" ++ (show_grps grps) ++ "]"
      where
        show_grps []       = ""
        show_grps (g:grps) =
          (show_grp g) ++ "\n" ++ (show_grps grps)
          where
            show_grp (LCP.Group lcp memb) = 
              "  " ++ (show lcp) ++ ": " ++ (Foldable.foldr fn "" memb) ++ "\n"
              where
                fn (LCP.GroupElem a b) sum = "\n    " ++ (show (a,b)) ++ sum
               

    process str = do
      let 
        input = CR.fromList $ str ++ "!"
        sa    = CR.make input
        sufs  = CR.suffixes input sa
        lcps  = CR.array input sa
        lcp_strs = map (("        "++) . show) $ UV.toList lcps

      putStrLn $ "input: " ++ str
      putStrLn $ "suffixes: " ++ "\nsrc idx | lcp | suffix \n----------------------------" ++ "\n  " ++ (intercalate "\n  " (interleave lcp_strs sufs))
      putStrLn $ "groups: \n" ++ (show_groups $ LCP.mergedGroups sa lcps)

