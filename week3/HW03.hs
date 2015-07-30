module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st ident val ident' =
  if ident == ident' then val
                     else st ident'
empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

false :: Int
true  :: Int
boolToInt :: Bool -> Int
intToBool :: Int -> Bool

false = 0
true  = 1
boolToInt False = false
boolToInt True  = true
intToBool b = b == true

evalBop :: Bop -> Int -> Int -> Int
evalBop bop =
  case bop of
    Plus -> (+)
    Minus -> (-)
    Times -> (*)
    Divide -> div
    Gt -> mkBoolOp (>)
    Ge -> mkBoolOp (>=)
    Lt -> mkBoolOp (<)
    Le -> mkBoolOp (<=)
    Eql -> mkBoolOp (==)
  where
    mkBoolOp op a b = boolToInt ( a `op` b )

evalE :: State -> Expression -> Int
evalE st (Var ident)  = st ident
evalE _  (Val x)      = x
evalE st (Op a bop b) = evalE st a `op` evalE st b
                        where
                            op = evalBop bop

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v ex) = DAssign v ex
desugar (Incr v)      = DAssign v (Op (Var v) Plus (Val 1))
desugar (If p t f)    = DIf p (desugar t) (desugar f)
desugar (While p st)  = DWhile p (desugar st)
desugar (For i p u st) =   -- for( init; loop condition; update )
                           --    block
  DSequence (desugar i)         -- init
            (DWhile p           -- loop condition
              (DSequence
                (desugar st)    -- block
                (desugar u)))   -- update
desugar (Sequence a b)  = DSequence (desugar a) (desugar b)
desugar (Skip)          = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State

evalSimple st (DAssign v ex) =
  extend st v (evalE st ex)

evalSimple st (DIf p t f) =
  if evalE st p == true
    then evalSimple st t
    else evalSimple st f

evalSimple state (DWhile p block) =
  aux state
  where
    aux st =
      if evalE st p == true
        then aux (evalSimple st block)
        else st

evalSimple st (DSequence a b) =
  let st' = evalSimple st a in
  evalSimple st' b

evalSimple st (DSkip) =
  st

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   Out := 0;
   while (In >= Out * Out) {
     Out++
   };
   Out := Out - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 0;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 0)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
