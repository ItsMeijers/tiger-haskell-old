import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Control.Monad (mplus)

-- Chapter 1

-- Program Section

type Id = String

data BinOp = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp

main :: IO ()
main = interp program

-- a := 5 + 3 ; b := ( print ( a, a - 1) , 10 * a) ; print (b)
program :: Stm
program = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                      (CompoundStm
                        (AssignStm "b" (EseqExp
                                          (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                          (OpExp (NumExp 10) Times (IdExp "a"))))
                        (PrintStm [IdExp "b"]))

-- Tells the maximum number of arguments of any 'print' statement within any
-- subexpression of a given statement
maxArgs :: Stm -> Int
maxArgs (CompoundStm stm stm1) = maxArgs stm + maxArgs stm1
maxArgs (AssignStm _ expr)     = maxArgsExp expr
maxArgs (PrintStm es)          = 1 + sum (map maxArgsExp es)

-- Helper to evaluate exp for maxArgs
maxArgsExp :: Exp -> Int
maxArgsExp (OpExp expr _ expr1) = maxArgsExp expr + maxArgsExp expr1
maxArgsExp (EseqExp stm exp1)   = maxArgs stm + maxArgsExp exp1
maxArgsExp _                    = 0

interp :: Stm -> IO ()
interp stm = print (interpStm stm [])

type Table = [(Id, Int)]

updateTable :: Id -> Int -> Table -> Table
updateTable i i' ts = (i, i') : ts

interpStm :: Stm -> Table -> Table
interpStm (CompoundStm stm stm1) t = (interpStm stm1 . interpStm stm) t
interpStm (AssignStm id' expr)   t = let (i, t') = interpExp expr t in (id', i) : t'
interpStm (PrintStm exps)        t = foldl fExp t exps
  where fExp t' expr = let (i, nt) = interpExp expr t' in trace (show i) nt


opExp :: BinOp -> Int -> Int -> Int
opExp Plus  = (+)
opExp Minus = (-)
opExp Times = (*)
opExp Div   = div

interpExp :: Exp -> Table -> (Int, Table)
interpExp (IdExp id')           t = (fromJust (lookup id' t), t)
interpExp (NumExp i)            t = (i, t)
interpExp (EseqExp stm expr)    t = interpExp expr (interpStm stm t)
interpExp (OpExp expr op expr1) t = let (i, t')  = interpExp expr t
                                        (i', nt) = interpExp expr1 t'
                                    in (opExp op i i', nt)

-- Exercises Section

type Key = String

data Tree a = Leaf | Node (Tree a) Key a (Tree a)
    deriving (Show)

empty :: Tree a
empty = Leaf

insert :: Key -> Tree a -> a -> Tree a
insert key Leaf a = Node Leaf key a Leaf
insert key (Node l k a' r) a
  | key < k   = Node (insert key l a) k a' r
  | key > k   = Node l k a' (insert key r a)
  | otherwise = Node l key a' r

member :: Key -> Tree a -> Bool
member _ Leaf = False
member key (Node l k a r)
  | key == k  = True
  | otherwise = member key l || member key r

lookupInTree :: Tree a -> Key -> Maybe a
lookupInTree Leaf _ = Nothing
lookupInTree (Node l k a r) key
  | k == key  = Just a
  | otherwise = lookupInTree l key `mplus` lookupInTree r key
