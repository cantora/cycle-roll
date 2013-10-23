module Main where

import qualified Data.CycleRoll as CR
import qualified Data.CycleRoll.LCP as LCP
import qualified Data.CycleRoll.SuffixArray as SA
import qualified Data.CycleRoll.Sequence as Seq

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
          (show g) ++ "\n" ++ (show_grps grps)
               

    process str = do
      let 
        input = UV.fromList $ str ++ "!"
        sa    = SA.make input
        sufs  = CR.suffixes input sa
        lcps  = LCP.array input sa
        merged_grps = LCP.mergedGroups $ LCP.groups sa lcps
        lcp_strs = map (("        "++) . show) $ UV.toList lcps
        seqs  = Seq.sequences merged_grps $ LCP.rmq lcps

      putStrLn $ "input: " ++ str
      putStrLn $ "suffixes: " ++ "\nsrc idx | lcp | suffix \n----------------------------" ++ "\n  " ++ (intercalate "\n  " (interleave lcp_strs sufs))
      putStrLn $ "groups: \n" ++ (show_groups merged_grps)

      putStrLn $ "seqs: \n  " ++ (intercalate "\n  " $ map show seqs)