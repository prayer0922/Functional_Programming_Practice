-- Zixuan You, zyou@ucsc.edu


-- Necessary imports
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Data.Char


--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | For Variable Expression Expression Statement -- For var in e1 to e2 do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- P.identifier lexer
              P.reserved lexer "in"
              low <- exprParser
              P.reserved lexer "to"
              up <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (For var low up body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing

miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)

-- Solution for Question 1
instance Show Value where
  show (IntVal x) = show x
  show (BoolVal x) = Prelude.map toLower (show x) 

instance Show Op where
  show Plus = " + "
  show Minus = " - "
  show Times = " * "
  show GreaterThan = " > "
  show Equals = " == "
  show LessThan = " < "

instance Show Expression where
  show (Var v) = v
  show (Val v) = show v
  show (BinOp op e1 e2) = show e1 ++ show op ++ show e2
  show (Assignment v e) = v ++ " = " ++ show e

instance Show Statement where
  show (Expr e) = show e
  show (If e s1 s2) = "if " ++ show e ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " end "
  show (While e s) = "while " ++ show e ++ " do " ++ show s  ++ " end "
  show (For v e1 e2 s) = "for " ++ v ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show s ++ " end "
  show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
  show Skip = ""


-- Solution for Question 2
evalE :: Expression -> Store -> (Value, Store)
evalE (BinOp o a b) s = (applyOp o a' b', s'')
    where (a', s')  = evalE a s
          (b', s'') = evalE b s'
-- eval variable: lookup x, if Nothing: error message, otherwise: use id to extract result inside Just
evalE (Var x) s = let result = maybe (error "The variable is not found!") id (Data.Map.lookup x s)
                  in  (result, s)  
evalE (Val v) s = (v,s)
evalE (Assignment x e) s = let result = (evalE e s) in (fst result, insert x (fst result) (snd result))

-- Solution for Question 3
evalS :: Statement -> Store -> Store
evalS w@(While e s1) s = case (evalE e s) of
                          (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                          (BoolVal False,s') -> s'
                          _                  -> error "Condition must be a BoolVal"
-- For loop: 1. assign v to e1 under s, 2. While (v < e2) do s1;v++, 3. If (v == e2) then s1;v++ else Skip
evalS (For v e1 e2 s1) s = let s' = snd (evalE (Assignment v e1) s)
                               s'' = (evalS (While (BinOp LessThan (Var v) e2) (Sequence s1 (Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1))))))) s')
                           in  evalS (If (BinOp Equals (Var v) e2) (Sequence s1 (Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1)))))) Skip) s''
evalS Skip s             = s
evalS (Expr e) s         = snd (evalE e s)
evalS (Sequence s1 s2) s = let s' = evalS s1 s 
                           in  evalS s2 s'
evalS (If e s1 s2) s     = case (evalE e s) of
                             (BoolVal True, s')  -> evalS s1 s'
                             (BoolVal False, s') -> evalS s2 s'
                             _                   -> error "Condition must be a BoolVal"

-- Solutino for Question 4
evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s          = (\v -> (v,s)) <$> (Data.Map.lookup x s)
evalE_maybe (Val v) s          = Just (v,s)
-- If e evaluates to Nothing, then Nothing, else insert result into s
evalE_maybe (Assignment x e) s = (fst <$> evalE_maybe e s) >>= (\r -> Just (r, insert x r s))

evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe (While e s1) s     = case (evalE_maybe e s) of
                                   Just (BoolVal True, s')  -> evalS_maybe (Sequence s1 (While e s1)) s'
                                   Just (BoolVal False, s') -> Just s'
                                   _                        -> Nothing
-- For loop: 1. Assignment v e1, 2.  While (v < e2) do s1;v++, 3. If (v == e2) then s1;v++ else Skip
evalS_maybe (For v e1 e2 s1) s = let body = Sequence s1 (Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1)))))     -- body of for loop is s1;v++
                                 in  (evalE_maybe (Assignment v e1) s) >>=
                                     (\(_,s') -> (evalS_maybe (While (BinOp LessThan (Var v) e2) body) s')) >>=
                                     (\s'' -> evalS_maybe (If (BinOp Equals (Var v) e2) body Skip) s'')
evalS_maybe Skip s             = Just s
evalS_maybe (Sequence s1 s2) s = evalS_maybe s1 s >>= (\s' -> evalS_maybe s2 s')
evalS_maybe (Expr e) s         = snd <$> (evalE_maybe e s)
evalS_maybe (If e s1 s2) s     = case (evalE_maybe e s) of
                                   Just (BoolVal True, s')  -> evalS_maybe s1 s'
                                   Just (BoolVal False, s') -> evalS_maybe s2 s'
                                   _                        -> Nothing

-- Solution for 5
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\_ -> Nothing)


evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Var x) = getVar x 
evalE_monad (Val v) = return v
evalE_monad (Assignment x e) = do res <- evalE_monad e
                                  setVar x res

evalS_monad :: Statement -> Imperative ()
evalS_monad (While e s1)     = do res <- evalE_monad e
                                  case res of
                                    (BoolVal False) -> evalS_monad Skip
                                    (BoolVal True) -> evalS_monad (Sequence s1 (While e s1))
                                    _ -> fail "Condition must be a BoolVal"
evalS_monad (For v e1 e2 s1) = let body = (Sequence s1 (Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1))))))
                               in
                               do evalE_monad (Assignment v e1)
                                  evalS_monad (While (BinOp LessThan (Var v) e2) body)
                                  evalS_monad (If (BinOp Equals (Var v) e2) body Skip)
evalS_monad Skip             = return ()
evalS_monad (Sequence s1 s2) = do evalS_monad s1
                                  evalS_monad s2
evalS_monad (Expr e)         = do evalE_monad e
                                  evalS_monad Skip
evalS_monad (If e s1 s2)     = do res <- evalE_monad e
                                  case res of
                                    (BoolVal False) -> evalS_monad s2
                                    (BoolVal True) -> evalS_monad s1
                                    _ -> fail "Condition must be a BoolVal"


getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))
-- Solution for 6 is added to each part.



-- Test cases
main = do putStrLn "Test cases for Question 1"
          pp "1 + 1"
          pp "23*x<42"
          pp "if false then x=2 else x = 3 end ; x = x + 2"
          pp "x = 1; while x < 5 do x = x + 1 end"
          putStrLn "\nTest cases for Question 2"
          putStrLn (show $ evalE (Val (BoolVal True)) Data.Map.empty )
          run exprParser "1+1" evalE      
          run exprParser "13*2 < 27" evalE
          putStrLn (show $ evalE (Var "x") (fromList [("x",IntVal 23)]) )
          run exprParser "x = 23" evalE
          run exprParser "x = y = 2 + 3" evalE
          putStrLn "\nTest cases for Question 3"
          run stmtParser "x=1+1" evalS
          run stmtParser "x = 2; x = x + 3" evalS
          run stmtParser "if true then x = 1 else x = 2 end" evalS
          run stmtParser "x = 2; y = x + 3; if y < 4 then z = true else z = false end" evalS
          run stmtParser "x = 1; while x < 3 do x = x + 1 end" evalS
          run stmtParser "x = 1 ; y = 1; while x < 5 do x = x + 1 ; y = y * x end" evalS
          putStrLn "\nTest cases for Question 4"
          run exprParser "1+1" evalE_maybe
          run exprParser "10 < x + 1" evalE_maybe
          run exprParser "10 == 4 * 2" evalE_maybe
          run stmtParser "x = 2; y = z" evalS_maybe
          run stmtParser "x = true; if x then y = 1 else y = 2 end" evalS_maybe
          run stmtParser "x = 1; if x then y = 1 else y = 2 end" evalS_maybe
          putStrLn "\nTest cases for Question 5"
          putStrLn (show $ runImperative miniprog empty)
          putStrLn (show $ runMonad "x = 1")
          putStrLn (show $ runMonad "x = 1; if x == 1 then y = 1 else y = 2 end")
          putStrLn (show $ runMonad "x = 1; if x == z then y = 1 else y = 2 end")
          putStrLn (show $ runMonad "while 23 x = x + 1 end")
          putStrLn "\nTest cases for Question 6"
          putStrLn (show $ (For "a" (Val (IntVal 1)) (Val (IntVal 2)) (Expr (Val (IntVal 3)))))
          putStrLn (show $ parse stmtParser "" "for x in 1 to 4 do y = x end")
          run stmtParser "for x in 1 to 4 do y = x end" evalS
          run stmtParser "for x in 1 to 4 do y = x end" evalS_maybe
          run stmtParser "for x in 1 to 4 do y = z end" evalS_maybe
          putStrLn (show $ runMonad "for x in 1 to 4 do y = x end")
          putStrLn (show $ runMonad "for x in 1 to 4 do y = z end") 
          
          
          
