module Main where

import qualified Data.CycleRoll.Internal.LCP as LCP
import qualified Data.CycleRoll.Internal.SuffixArray as SA
import qualified Data.CycleRoll.Internal.Sequence as Seq
import qualified Data.CycleRoll.Internal.Algorithm as Algorithm
import qualified Data.CycleRoll.RSequence as RSequence

import System.Environment
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Heap as Heap
import qualified Data.Foldable as Foldable

interleave _ [] = []
interleave [] xs = xs
interleave (x1:xs1) (x2:[]) = [x2]
interleave (x1:xs1) (x2:xs2) = [x2,x1] ++ (interleave xs1 xs2)

suffixes :: UV.Vector Char -> UV.Vector Int -> [[Char]]
suffixes data_v suf_arr =
  process suf_arr
  where
    process sa 
      | sa == UV.empty   = []
      | otherwise        = ((show d_idx) ++ "\t\t" ++ sufx) : (process $ UV.tail sa)
      where
        d_idx = UV.head sa
        sufx  = UV.toList $ SA.entry' data_v d_idx

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
        input       = UV.fromList $ str ++ "!"
        sa          = SA.make input
        sufs        = suffixes input sa
        lcps        = LCP.array input sa
        merged_grps = LCP.mergedGroups $ LCP.groups sa lcps
        lcp_strs    = map (("        "++) . show) $ UV.toList lcps
        seqs        = Seq.sequences (UV.length input) merged_grps $ LCP.rmq lcps
        all_seqs    = concat $ map Foldable.toList $ seqs
        result      = Algorithm.roll (UV.length input) seqs
        -- display result = map disp_rseq result

      putStrLn $ "input: " ++ str
      putStrLn $ "suffixes: " ++ "\nsrc idx | lcp | suffix \n----------------------------" ++ "\n  " ++ (intercalate "\n  " (interleave lcp_strs sufs))
      putStrLn $ "groups: \n" ++ (show_groups merged_grps) ++ "\n"

      putStrLn $ "seqs: \n  " ++ (intercalate "\n  " $ map show seqs) ++ "\n"
      putStrLn $ "result: \n  " ++ (intercalate "\n  " $ map show result) ++ "\n"
      putStrLn $ RSequence.displayList input result
