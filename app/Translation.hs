module Translation where

import Epi
import Butf
import Fresh
import Control.Monad
import Control.Monad.List
-- Helpers 
tname :: String -> Epi.Term
tname x = TChan (Name x)
tvar :: String -> Epi.Term
tvar x = TChan (Variable x)
zipWith3M f xs ys zs =
  zipWithM (\x -> \(y,z) -> (f x y z)) xs (zip ys zs)
-- Translate a Butf expression to an Epi process
-- Update the type signature to include NameGenerator
translateToEpi :: Butf.Expr -> String -> NameGenerator Epi.Process
translateToEpi (Const n) o = return $ Send (Name o) [Number n] Nul
translateToEpi (Var x) o = return $ Send (Name o) [tname x] Nul
translateToEpi (Lambda x e) o = do
    h <- freshName "h" 
    r <- freshName "r"
    translatedBody <- translateToEpi e r  -- Translate body expression with the return channel r
    return $ Res h (Par (Send (Name o) [tname h] Nul) (Rep (Recv (Name h) [x, r] translatedBody)))
translateToEpi (If e1 e2 e3) o = do
    o1 <- freshName "o" 
    v <- freshName "v"
    cond <- translateToEpi e1 o1
    _then <-  translateToEpi e2 o
    _else <- translateToEpi e3 o
    return $ Res o1 (
        Par (
            cond
        ) (
            Recv (Name o1) [v] (
                Match (tvar v) (Number 0) Neq (
                    _then
                ) (
                    _else
                )
            )
        ))

translateToEpi (App e1 e2) o = do
    o1 <- freshName "o"  
    o2 <- freshName "o" 
    h <- freshName  "h" 
    v <- freshName  "v" 
    e1' <- translateToEpi e1 o1 
    e2' <- translateToEpi e2 o2 
    return $ Res o1 (Res o2 (
            Par (
                e1'
            ) (
                Par (
                    e2'
                ) (
                    Recv (Name o1) [h] ( 
                        Recv (Name o2) [v] (
                            Send (Name h) [tvar v, tvar o] Nul
                        )
                    )
                )
            )
        ))
translateToEpi (Tuple es) o = do
    -- Generate fresh names for each element in the tuple
    let n = length es
    outputNames <- replicateM n (freshName "o") -- Generate names "o0", "o1", ..., "on"
    valueNames <- replicateM n (freshName "v")  -- Generate names "v0", "v1", ..., "vn"
    h <- freshName "h"
    -- Translate each element of the tuple with its corresponding fresh name
    elementProcesses <- zipWithM translateToEpi es outputNames
    -- Create the final process using the given pattern
    let tupleProcess = combineProcesses outputNames elementProcesses valueNames h o
    return tupleProcess

-- translateToEpi (Array es) o = do 
--    let n = length es

-- Helper function to combine the processes into the final tuple process
combineProcessesTuple :: [String]   -- Generated output channel names (o1, o2, ..., on)
                -> [Epi.Process]    -- Translated element processes
                -> [String]         -- Fresh variable names (v1, v2, ..., vn) for binding
                -> String           -- Fresh handle name (h)
                -> String           -- Final output channel (o)
                -> Epi.Process
combineProcessesTuple outputNames elementProcesses vs h o =
  let
    -- Pair each output channel with its corresponding variable
    outputVarPairs = zip outputNames vs

    -- Receive results using the generated variables
    receiveResults = foldr (\(chan, var) acc -> Recv (Name chan) [var] acc) Nul outputVarPairs

    -- Send the handle on the output channel
    sendHandle = Send (Name o) [tname h] Nul

    -- Create replicated send process for the tuple
    sendTuple = Rep (Send (Labelled (Name h) "tup") (map tvar vs) Nul)

    -- Combine receiver and sender
    receiverSender = (Par receiveResults (Res h (Par sendHandle sendTuple)))

    -- Parallel composition of all element processes
    unrestrictedTuple = foldr Par receiverSender elementProcesses
    
  in
    -- Combine everything with restrictions
    foldr Res (unrestrictedTuple ) outputNames

