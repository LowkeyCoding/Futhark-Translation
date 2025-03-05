module Translation where

import Butf
import Control.Monad
import Control.Monad.List
import Data.List
import Data.Strings
import Epi
import EpiPretty
import Fresh

tname :: String -> Epi.Term
tname x = TChan (Name x)

tvar :: String -> Epi.Term
tvar x = TChan (Variable x)

zipWith3M f xs ys zs =
  zipWithM (\x (y, z) -> f x y z) xs (zip ys zs)

translateToEpi :: Butf.Expr -> String -> NameGenerator (Epi.Process, String)
translateToEpi (Const n) o = do
  let process = Send (Name o) [Number n] Nul
      log = over (show n) process
  return (process, log)

translateToEpi (Var x) o = do
  let process = Send (Name o) [tname x] Nul
      log = over x process
  return $ (process, log)

translateToEpi (Array es) o = do
  let n = length es
  outputNames <- replicateM n (freshName "o")

  -- Translate elements and capture logs
  elements <- forM (zip es outputNames) $ \(e, name) -> (translateToEpi e name)
  let (elementProcesses, elementLogs) = unzip elements

  valueNames <- replicateM n (freshName "v")
  returnNames <- replicateM n (freshName "r")
  h <- freshName "h"

  -- Generate final process
  let receiveResults =
        foldr
          (\(c, v) acc -> Recv (Name c) [v] acc)
          Nul
          (zip outputNames valueNames)
      (cells, cellLogs) =
        foldr
          ( \(i, v, r) (accProc, accLogs) ->
              let (newProc, newLog) = cell h i v r
               in (Par newProc accProc, newLog : accLogs)
          )
          (Nul, []) -- Start with empty process and logs
          (zip3 [0 ..] valueNames returnNames)
      len = Rep (Send (Labelled (Name h) "len") [Number n] Nul)
      handle = (Send (Name o) [tname h] Nul)
      arrayProcess =
        foldr
          Res
          ( Res
              h
              ( Par
                  (foldr Par receiveResults elementProcesses)
                  (Par cells (Par len handle))
              )
          )
          outputNames

  -- Build component logs
  let receiverLog = over "receiver" receiveResults
      lenLog = over "len" $ len
      handelLog = over "handel" $ handle
  let allLogs =  elementLogs ++ [receiverLog]  ++ cellLogs ++ [lenLog] ++ [handelLog]
  let restrictedLog = foldr (\name log -> "nu " ++ name ++ ".(" ++ log ++ ")") (intercalate "|" allLogs) (outputNames ++ [h])
  return (arrayProcess, restrictedLog ++ "\\ ")


translateToEpi (Index e1 e2) o = do
  o1 <- freshName "o"
  o2 <- freshName "o"
  h <- freshName "h"
  i <- freshName "i"
  _i <- freshName "i"
  v <- freshName "v"
  
  -- Translate array and index expressions with logs
  (e1', arrayLogs) <- translateToEpi e1 o1
  (e2', indexLogs) <- translateToEpi e2 o2

  -- Build index access components
  let handelProcess = (Send (Name o) [tname v] Nul)
      matchProcess  = Match (tname i) (Number 0) Geq
                     (Recv (Labelled (Name h) i) [_i, v] 
                     handelProcess)
                     Nul
      receiveProcess = Recv (Name o1) [h] 
                     (Recv (Name o2) [i] 
                     matchProcess)

  -- Create component logs
  let accessLog = over "access" receiveProcess

  -- Combine all logs
  let allLogs =  [arrayLogs,indexLogs,accessLog]
  
  -- Build final process with restrictions
  let finalProcess = Res o1 (Res o2 
                   (Par e1' (Par e2' receiveProcess)))

  -- Add restrictions to log
  let restrictedLog = foldr (\n log -> "nu " ++ n ++ ".(" ++ log ++ ")")
                   (intercalate "|" allLogs)
                   [o1, o2]
  return (finalProcess, restrictedLog ++ "\\ ")

translateToEpi (Lambda x e) o = do
    h <- freshName "h"
    r <- freshName "r"
    (e', bodyLog) <- translateToEpi e r
    let lambda = (\y -> Res h (Par (Send (Name o) [tname h] Nul) (Rep (Recv (Name h) [x, r] y))))
        log = under ("lambda " ++ h) (prettyProcess $ lambda (Res "e'" Nul))
        logS = sReplace ("nu e'.(null)") (under "body" bodyLog) log
    return (lambda e', logS)

translateToEpi (BinOp op e1 e2) o = do 
    o1 <- freshName "o"
    o2 <- freshName "o"
    v1 <- freshName "v"
    v2 <- freshName "v"
    (e1', v1Logs) <- translateToEpi e1 o1
    (e2', v2Logs) <- translateToEpi e2 o2
    
    let binop = (Send (Name o) [BinaryOp op (tvar v1) (tvar v2)] Nul)
        receiver  = (\x -> (Recv (Name o1) [v1] (Recv (Name o2) [v2] x))) 
        full = (\x y z -> Res o1 (Res o2 (Par x (Par y z))))
        log = (prettyProcess $ full (Res "e1'" Nul) (Res "e2'" Nul) (Res "receiver" Nul))
        logS1 = sReplace "nu e1'.(null)" v1Logs log
        logS2 = sReplace "nu e2'.(null)" v2Logs logS1
        logS3 = sReplace "nu receiver.(null)" (over "receiver" (receiver (Res "binop" Nul))) logS2
        logS4 = sReplace "nu binop.(null),\"receiver\")" (",\"receiver\")" ++ over "binop" binop) logS3
    return (full e1' e2' (receiver binop),logS4)

-- Helper functions
cell :: String -> Int -> String -> String -> (Epi.Process, String)
cell h i v r =
  let process =
        Par
          (Rep (Recv (Labelled (Name h) "all") [r] (Send (Variable r) [Number i, tvar v] Nul)))
          (Rep (Send (Labelled (Name h) (show i)) [Number i, tvar v] Nul))
      log = over ("cell_" ++ show i) process
   in (process, log)
