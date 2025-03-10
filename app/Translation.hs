module Translation where

import Binop
import Butf
import Control.Monad
import Control.Monad.List
import Data.List
import Data.Strings
import Data.Type.Equality (trans)
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
  elements <- forM (zip es outputNames) $ uncurry translateToEpi
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
      handle = Send (Name o) [tname h] Nul
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
      lenLog = over "len" len
      handleLog = over "handle" handle
  let allLogs = elementLogs ++ [receiverLog] ++ cellLogs ++ [lenLog] ++ [handleLog]
  let restrictedLog = foldr (\name log -> "nu " ++ name ++ ".(" ++ log ++ ")") (intercalate "|" allLogs) (outputNames ++ [h])
  return (arrayProcess, "undershell("++restrictedLog++",\"array\")")

translateToEpi (Tuple es) o = do
    let n = length es
    outputNames <- replicateM n (freshName "o")
    valueNames <- replicateM n (freshName "v")  -- Ensure value names are distinct
    h <- freshName "h"
    elements <- forM (zip es outputNames) $ uncurry translateToEpi
    let (elementProcesses, _elementLogs) = unzip elements
        elementLogs = map (++"\\ ") _elementLogs

    let receiverSenderProcess = 
            Par (foldr (\(c,v) a -> Recv (Name c) [v] a) Nul (zip outputNames valueNames))
                (Res h (Par (Send (Name o) [tname h] Nul) 
                      (Rep (Send (Labelled (Name h) "tup") (map tvar valueNames) Nul))))
        process = foldr Res (foldr Par receiverSenderProcess elementProcesses) outputNames

        -- Collect component logs
        receiverLog = over "receiver" (foldr (\(c,v) a -> Recv (Name c) [v] a) Nul (zip outputNames valueNames))
        handleLog = over "handle" (Send (Name o) [tname h] Nul)
        repLog = over "rep" (Rep (Send (Labelled (Name h) "tup") (map tvar valueNames) Nul))
        allLogs = elementLogs ++ [receiverLog, handleLog, repLog]
        -- Apply restrictions to the combined logs H should be added earlier
        restrictedLog = foldr (\name log -> "nu " ++ name ++ ".(" ++ log ++ ")") (intercalate "|" allLogs) (outputNames ++ [h])

    return (process, "undershell("++restrictedLog++",\"tuple\")")

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
  let handleProcess = Send (Name o) [tname v] Nul
      matchProcess =
        Match
          (tname i)
          (Number 0)
          Geq
          ( Recv
              (Labelled (Name h) i)
              [_i, v]
              handleProcess
          )
          Nul
      receiveProcess =
        Recv
          (Name o1)
          [h]
          ( Recv
              (Name o2)
              [i]
              matchProcess
          )

  -- Create component logs
  let accessLog = over "access" receiveProcess

  -- Combine all logs
  let allLogs = [arrayLogs, indexLogs, accessLog]

  -- Build final process with restrictions
  let finalProcess =
        Res
          o1
          ( Res
              o2
              (Par e1' (Par e2' receiveProcess))
          )

  -- Add restrictions to log
  let restrictedLog =
        foldr
          (\n log -> "nu " ++ n ++ ".(" ++ log ++ ")")
          (intercalate "|" allLogs)
          [o1, o2]
  return (finalProcess, restrictedLog ++ "\\ ")

translateToEpi (Lambda x e) o = do
  h <- freshName "h"
  r <- freshName "r"
  (e', bodyLog) <- translateToEpi e r
  let lambda = (\y -> Res h (Par (Send (Name o) [tname h] Nul) (Rep (Recv (Name h) [x, r] y))))
      log = under ("lambda " ++ h) (prettyProcess $ lambda (Res "e'" Nul))
      logS = sReplace "nu e'.(null)" (under "body" bodyLog) log
  return (lambda e', logS)

translateToEpi (App e1 e2) o = do
  o1 <- freshName "o"
  o2 <- freshName "o"
  f <- freshName "f"
  v <- freshName "v"
  (e1', v1Logs) <- translateToEpi e1 o1
  (e2', v2Logs) <- translateToEpi e2 o2

  let func = Send (Name f) [tvar f, tvar v] Nul
      receiver = (\x -> (Recv (Name o1) [f] (Recv (Name o2) [v] x)))
      log = prettyProcess $ full (Res "e1'" Nul) (Res "e2'" Nul) (Res "receiver" Nul)
      logS1 = sReplace "nu e1'.(null)" v1Logs log
      logS2 = sReplace "nu e2'.(null)" v2Logs logS1
      logS3 = sReplace "nu receiver.(null)" (over "receiver" (receiver (Res "func" Nul))) logS2
      logS4 = sReplace "nu func.(null),\"receiver\")" (",\"receiver\")" ++ over "func" func) logS3
      full x y z = Res o1 (Res o2 (Par x (Par y z)))
  return (full e1' e2' (receiver func), logS4)

translateToEpi (If e1 e2 e3) o = do
  o1 <- freshName "o"
  v <- freshName "v"

  (e1', e1Logs) <- translateToEpi e1 o1
  (e2', e2Logs) <- translateToEpi e2 o
  (e3', e3Logs) <- translateToEpi e3 o

  let _if = \x y z -> Res o1 (Par x (Recv (Name o1) [v] (Match (tvar v) (Number 1) Neq y z)))
      log1 = sReplace "nu e1'.(null)" e1Logs (prettyProcess (_if (Res "e1'" Nul) (Res "e2'" Nul) (Res "e3'" Nul)))
      log2 = sReplace "nu e2'.(null)" e2Logs log1
      log3 = sReplace "nu e3'.(null)" e3Logs log2
  return (_if e1' e2' e3', log3)

translateToEpi (BinOp op e1 e2) o = do
  o1 <- freshName "o"
  o2 <- freshName "o"
  v1 <- freshName "v"
  v2 <- freshName "v"
  (e1', v1Logs) <- translateToEpi e1 o1
  (e2', v2Logs) <- translateToEpi e2 o2

  let binop = Send (Name o) [BinaryOp op (tvar v1) (tvar v2)] Nul
      receiver = (\x -> (Recv (Name o1) [v1] (Recv (Name o2) [v2] x)))
      log = prettyProcess $ full (Res "e1'" Nul) (Res "e2'" Nul) (Res "receiver" Nul)
      logS1 = sReplace "nu e1'.(null)" v1Logs log
      logS2 = sReplace "nu e2'.(null)" v2Logs logS1
      logS3 = sReplace "nu receiver.(null)" (over "receiver" (receiver (Res "binop" Nul))) logS2
      logS4 = sReplace "nu binop.(null),\"receiver\")" (",\"receiver\")" ++ over "binop" binop) logS3
      full x y z = Res o1 (Res o2 (Par x (Par y z)))
  return (full e1' e2' (receiver binop), logS4)

translateToEpi (Size e) o = do
  o1 <- freshName "o"
  h <- freshName "h"
  n <- freshName "n"
  (e', eLogs) <- translateToEpi e o1

  let size = \x -> Res o1 (Par x (Recv (Name o1) [h] (Recv (Labelled (Variable h) "len") [n] (Send (Name o) [tvar n] Nul))))
      log = sReplace "nu e'.(null)" eLogs (prettyProcess (size (Res "e'" Nul)))
  return (size e', log)

translateToEpi (Iota e) o = do
  o1 <- freshName "o"
  r <- freshName "r"
  d <- freshName "d"
  h <- freshName "h"
  n <- freshName "n"
  c <- freshName "c"
  n1 <- freshName "n"
  (e', eLogs) <- translateToEpi e o1

  let (r_proc, rlog) = repeater n r d c n
  let process = \x -> Res o1 (Res r (Res h (Par x (Par (Recv (Name o1) [n] r_proc) (Nul)))))
      log = sReplace "nu e'.(null)" eLogs (prettyProcess (process (Res "e'" Nul)))
  return (process e', log)

translateToEpi (Map e) o = do
  o1 <- freshName "o"
  o' <- freshName "o"
  args <- freshName "h"
  func <- freshName "f"
  arr <- freshName "h"
  n <- freshName "n"
  vals <- freshName "v"
  count <- freshName "r"
  done <- freshName "d"
  cpriv <- freshName "c"
  npriv <- freshName "n"
  index <- freshName "i"
  value <- freshName "v"
  fvalue <- freshName "v"
  freturn <- freshName "r"
  temp1 <- freshName "t"
  temp2 <- freshName "t"
  arr' <- freshName "h"
  rcpriv <- freshName "r" 
  let (r_proc, r_log) = repeater n count done cpriv npriv
  (e', eLogs) <- translateToEpi e o1
  let (c_proc, c_log) = _cell arr' index fvalue rcpriv
  let arr_len = (Rep (Send (Labelled (Name arr) "len") [tvar n] Nul))
      new_handle = (Send (Name o) [tname arr'] Nul)
      bound_check = \h -> (Send (Name func) [Number 0, tname o'] (Recv (Name done) [] h))
      r_bc = \bc -> Res o' bc
      apply_func = \cell -> (Send (Name func) [tname value, tname freturn] (Recv (Name freturn) [fvalue] (Recv (Name count) [temp1, temp2] cell)))
      r_af = \af -> Res freturn af
      get_values = \r_af -> Recv (Name vals) [index, value] r_af
      r_gv = \gv -> Rep gv
      cl = \x y -> Par x y
      ploop = \x y z -> Par x (Par y z)
      r_loop = \x -> Res count x 
      get_arrv = \x -> Recv (Labelled (Variable arr) "all") [vals] x
      r_get_arrv = \x -> Res vals x 
      get_args = \x -> (Recv (Name o1) [args] (Recv (Labelled (Variable args) "tup") [func, arr] (Recv (Labelled (Variable arr) "len") [n] x)))
      process = (\x y -> Res o1 (Par x y))
      -- logging
      new_handle_log = over "new array" new_handle
      arr_len_log = over "arry length" arr_len
      bound_check_log = prettyProcess (bound_check (hole "new_handle"))
      apply_func_log = prettyProcess (apply_func (hole "cell"))
      get_values_log = prettyProcess (get_values (hole "afr"))
      get_arrv_log = prettyProcess (get_arrv (hole "loop"))
      r_get_arrv_log = prettyProcess (r_get_arrv (hole "arv1"))
      get_args_log = prettyProcess (get_args (hole "arv"))
      process_log = prettyProcess (process (hole "e'") (hole "rest"))

      fcheck1 = ruhole "new_handle" "bound_check" (new_handle_log) bound_check_log
      fcheck = rhole "bc" fcheck1 (prettyProcess (r_bc (hole "bc")))
      cell_af1 = ruhole "cell" "apply func" ("("++c_log++")") apply_func_log
      cell_af2 = rhole "af" cell_af1 (prettyProcess (r_af (hole "af")))
      cell_af3 = rhole "afr" cell_af2 get_values_log
      cell_af = rhole "afrv" cell_af3 (prettyProcess (r_gv (hole "afrv")))
      check_len1 = rhole "al" arr_len_log (prettyProcess (cl (hole "bc") (hole "al")))
      check_len = rhole "bc" fcheck check_len1
      loop1 = rhole "count" r_log (prettyProcess (ploop (hole "count") (hole "cell_af") (hole "check_len"))) 
      loop2 = rhole "cell_af" ("\\ "++cell_af) loop1
      loop3 = rhole "check_len" ("\\ "++check_len) loop2 
      loop  = rhole "loop3" loop3 (prettyProcess (r_loop (hole "loop3")))
      arv1  = ruhole "loop" "elements" loop get_arrv_log
      arv   = rhole "arv1" arv1 r_get_arrv_log
      marg  = ruhole "arv" "arguments" arv get_args_log

      --logb4p = rhole "adad" logb2rv 
      final1 = rhole "e'" (eLogs) process_log
      final  = rhole "rest" (marg) final1
  return (process e' (get_args (r_get_arrv (get_arrv (r_loop (ploop r_proc (r_gv (get_values (r_af (apply_func  c_proc)))) (cl (bound_check new_handle) arr_len)))))) , final)   
-- Helper functions
hole :: String -> Process
hole x = (Res x Nul)

rhole :: String -> String -> String -> String
rhole x y z= sReplace ("nu " <> x <> ".(null)") y z

ruhole :: String -> String -> String -> String -> String
ruhole nu no pu r = "overshell("++ (sReplace ("nu "++ nu ++".(null)") (",\""<>no<>"\")" ++ pu) r)

cell :: String -> Int -> String -> String -> (Process, String)
cell h i v r =
  let process =
        Par
          (Rep (Recv (Labelled (Name h) "all") [r] (Send (Variable r) [Number i, tvar v] Nul)))
          (Rep (Send (Labelled (Name h) (show i)) [Number i, tvar v] Nul))
      log = over ("cell_" ++ show i) process
   in (process, log)

_cell :: String -> String -> String -> String -> (Process, String)
_cell h i v r =
  let process =
        Par
          (Rep (Recv (Labelled (Name h) "all") [r] (Send (Variable r) [tvar i, tvar v] Nul)))
          (Rep (Send (Labelled (Name h) (show i)) [tvar i, tvar v] Nul))
      log = over ("cell_" ++ show i) process
   in (process, log)


repeater :: String -> String -> String -> String -> String -> (Process, String)
repeater s r d c n =
  let process = Res c (Par (Recv (Name c) [n] (Match (tvar n) (Number 0) Gt (Par (Send (Name r) [BinaryOp Sub (tvar n) (Number 1), BinaryOp Sub (tvar n) (Number 1)] Nul) (Send (Name c) [BinaryOp Sub (tvar n) (Number 1)] Nul)) (Send (Name d) [] Nul))) (Send (Name c) [tvar s] Nul))
      log = over "Repeater" process
   in (process, log)
