
{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, GADTs #-}

--
--  Let: Full Memoized
--      
--    Name Analysis: Memoized Algol68
--              (recursive lookup funs)
--                   Memoized construction of Algol AST
--                   Memoized calculate
--

module LetSem_Algol68_mBIn_Rec_FullMemo where
import Language.Grammars.ZipperAG
import Control.Monad.State.Lazy
import Data.Generics.Zipper
import Data.Dynamic
import Data.Maybe
import Data.Data

import LetShared as Let

import Debug.Trace

import AlgolShared as Algol hiding ((.$>),(.$<))
import AlgolSem_mBIn_Rec_Memo hiding (Constructor, constructor, C_Root, Symboltable, Dir, MemoAG, Local, Child, count, Child_Right, Child_Left, Parent, updateMemoTree, lexemeMemoTable, MemoTree, lookupAttr, Att, MemoTable, emptyMemoTable, updateAttr)

lexeme_ConsAssign_1 :: Zipper a -> String
lexeme_ConsAssign_1 ag = case (getHole ag :: Maybe List) of
                            Just(ConsAssign v _ _) -> v
                            _ -> error "Error in lexeme_ConsAssign_1!"

lexeme_ConsLet_1 :: Zipper a -> String
lexeme_ConsLet_1 ag = case (getHole ag :: Maybe List) of
                         Just(ConsLet v _ _) -> v
                         _ -> error "Error in lexeme_ConsLet!"

lexeme_Variable :: Zipper a -> String
lexeme_Variable ag = case (getHole ag :: Maybe A) of
                       Just (Variable s) -> s
                       _ -> error "Error in lexeme_Variable!"

lexeme_Constant :: Zipper a -> Int
lexeme_Constant ag = case (getHole ag :: Maybe A) of
                       Just (Constant s) -> s
                       _ -> error "Error in lexeme_Constant!"

data Constructor = C_Root
                 | C_Let
                 | C_In
                 | C_ConsLet
                 | C_ConsAssign
                 | C_EmptyList
                 | C_Plus
                 | C_Minus
                 | C_Time
                 | C_Divide
                 | C_Constant
                 | C_Variable

constructor :: Zipper a -> Constructor
constructor ag =  case (getHole ag :: Maybe Let.Root) of
                    Just (Let.Root _) -> C_Root
                    _ -> case (getHole ag :: Maybe Let.Let) of
                           Just (Let.Let _ _) -> C_Let
                           _ -> case (getHole ag :: Maybe Let.In) of
                                  Just (Let.In _) -> C_In
                                  _ -> case (getHole ag :: Maybe Let.List) of
                                         Just (Let.ConsLet _ _ _   ) -> C_ConsLet
                                         Just (Let.ConsAssign _ _ _) -> C_ConsAssign
                                         Just (Let.EmptyList       ) -> C_EmptyList
                                         _ -> case (getHole ag :: Maybe Let.A) of
                                                Just (Let.Plus _ _  ) -> C_Plus
                                                Just (Let.Minus _ _ ) -> C_Minus
                                                Just (Let.Time _ _  ) -> C_Time
                                                Just (Let.Divide _ _) -> C_Divide
                                                Just (Let.Constant _) -> C_Constant
                                                Just (Let.Variable _) -> C_Variable
                                                _ -> error "Error in constructor!!"

---- Synthesized Attributes ----
-- Name Analysis via Algol68 scope rules

algol68 :: Zipper Let.Root -> Algol.Its
algol68 ag = case (constructor ag) of
          C_Root       -> algol68 $ ag.$1
          C_Let        -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_In         -> algol68 $ ag.$1
          C_ConsAssign ->
           concatIts
            (Algol.ConsIts (Algol.Decl (lexeme_ConsAssign_1 ag))
                           (algol68 $ ag.$2))
            (algol68 $ ag.$3)
          C_ConsLet    -> Algol.ConsIts
                        (Algol.Decl (lexeme_ConsLet_1 ag))
                    (Algol.ConsIts  
                           (Algol.Block (algol68 $ ag.$2))
                   (algol68 $ ag.$3) 
                   )

          C_EmptyList  -> Algol.NilIts
          C_Plus       -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Divide     -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Minus      -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Time       -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Variable   -> Algol.ConsIts (Algol.Use (lexeme_Variable ag)) Algol.NilIts 
          C_Constant   -> Algol.NilIts

errsAlgol :: Zipper Let.Root -> [String]
errsAlgol ag = case (constructor ag) of
                 C_Root       -> AlgolSem_mBIn_Rec_Memo.semantics (Algol.Root (algol68 ag))

--ag = toZipper $ LetSem_Algol68_mBIn_Rec_FullMemo.buildMemoTree t
--                  in  fst $ runState (calculateMemo Local) ag

---- Calculate the result of evaluating Let ----
calculate :: Zipper Let.Root -> Int
calculate ag = case (constructor ag) of
              C_Root       -> let tree = case (getHole ag :: Maybe Let.Root) of
                                            Just (t) -> t
                                            _        -> error "Error in calculate"
                                  errors = fst $ runState (errsAlgolMemo Local) (toZipper $ LetSem_Algol68_mBIn_Rec_FullMemo.buildMemoTree tree)
                              in if (length errors /= 0)
                                 then 0
                                 else calculate $ ag.$1
              C_Let        -> calculate $ ag.$2
              C_In         -> calculate $ ag.$1
              C_Plus       -> (calculate $ ag.$1) + (calculate $ ag.$2)
              C_Divide     -> div (calculate $ ag.$1) (calculate $ ag.$2)
              C_Minus      -> (calculate $ ag.$1) - (calculate $ ag.$2)
              C_Time       -> (calculate $ ag.$1) * (calculate $ ag.$2)
              C_Variable   -> upGetVarValue (lexeme_Variable ag) ag
              C_Constant   -> lexeme_Constant ag

upGetVarValue :: String -> Zipper Let.Root -> Int
upGetVarValue name ag = case (constructor ag) of
                       C_Root -> downGetVarValue name (ag.$1)
                       C_Let  -> downGetVarValue name ag
                       _ -> upGetVarValue name (parent ag)

downGetVarValue :: String -> Zipper Let.Root -> Int
downGetVarValue name ag = case (constructor ag) of
                          C_Let        -> downGetVarValue name (ag.$1)
                          C_ConsAssign -> if   (lexeme_ConsAssign_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_ConsLet    -> if   (lexeme_ConsLet_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_EmptyList  -> oneUpGetVarValue name ag

oneUpGetVarValue :: String -> Zipper Let.Root -> Int
oneUpGetVarValue name ag = case (constructor ag) of
                       C_Let -> upGetVarValue name (parent ag)
                       _   -> oneUpGetVarValue name (parent ag)


semantics :: Let.Root -> Int
semantics p = calculate $ (toZipper p)


nameAnalysis :: Let.Root -> [String]
nameAnalysis p = errsAlgol $ (toZipper p)


evaluator :: Let.Root -> (Int, [String])
evaluator p = (LetSem_Algol68_mBIn_Rec_FullMemo.semantics p , nameAnalysis p)






---- HOAH here

-- New structure to keep the calculated values
-- and supporting functions
data MemoTree = Memo_Root       MemoTree                    MemoTable
              | Memo_Let        MemoTree MemoTree           MemoTable
              | Memo_In         MemoTree                    MemoTable
              | Memo_ConsLet    String   MemoTree MemoTree  MemoTable
              | Memo_ConsAssign String   MemoTree MemoTree  MemoTable
              | Memo_EmptyList                              MemoTable
              | Memo_Plus       MemoTree MemoTree           MemoTable
              | Memo_Minus      MemoTree MemoTree           MemoTable
              | Memo_Time       MemoTree MemoTree           MemoTable
              | Memo_Divide     MemoTree MemoTree           MemoTable
              | Memo_Variable   String                      MemoTable
              | Memo_Constant   Int                         MemoTable
  deriving (Typeable, Data)

instance Show MemoTree where
   show = showMemoTree

showMemoTree (Memo_Root l _) = showMemoTree l
showMemoTree (Memo_Let l i _) = "let { " ++ showMemoTree l ++ "} in " ++  (showMemoTree i)
showMemoTree (Memo_In a _)     = showMemoTree a
showMemoTree (Memo_ConsLet s l ll _)    = s ++ " = " ++ showMemoTree l ++ ";\n " ++ showMemoTree ll
showMemoTree (Memo_ConsAssign s a ll _) = s ++ " = " ++ showMemoTree a   ++ ";\n " ++ showMemoTree ll 
showMemoTree (Memo_Plus al ar _)   = (showMemoTree al) ++ " + " ++ (showMemoTree ar)
showMemoTree (Memo_Minus al ar _)  = (showMemoTree al) ++ " - " ++ (showMemoTree ar)
showMemoTree (Memo_Time al ar _)   = (showMemoTree al) ++ " * " ++ (showMemoTree ar)
showMemoTree (Memo_Divide al ar _) = (showMemoTree al) ++ " / " ++ (showMemoTree ar)
showMemoTree (Memo_Variable s _)   = s 
showMemoTree (Memo_Constant i _)   = show i
showMemoTree _                   = ""

data Att a where
   Attr_algol68          :: Att Algol.Its
   Attr_errsAlgol        :: Att [String]
   Attr_calculate        :: Att Int
   Attr_upGetVarValue    :: Att Int
   Attr_downGetVarValue  :: Att Int
   Attr_oneUpGetVarValue :: Att Int

type MemoTable = ( Maybe Algol.Its
                 , Maybe [String]
                 , Maybe Int
                 , Maybe Int
                 , Maybe Int
                 , Maybe Int
                 )

emptyMemoTable :: MemoTable
emptyMemoTable = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

data Dir = Parent | Child Int | Local | Child_Right Int | Child_Left Int
type MemoAG = State (Zipper MemoTree)

lexemeMemoTable :: Zipper MemoTree -> MemoTable
lexemeMemoTable memo_ag = case (getHole memo_ag :: Maybe MemoTree) of
                            Just (Memo_Root       _     table) -> table
                            Just (Memo_Let        _ _   table) -> table
                            Just (Memo_In         _     table) -> table
                            Just (Memo_ConsLet    _ _ _ table) -> table
                            Just (Memo_ConsAssign _ _ _ table) -> table
                            Just (Memo_EmptyList        table) -> table
                            Just (Memo_Plus       _ _   table) -> table
                            Just (Memo_Minus      _ _   table) -> table
                            Just (Memo_Time       _ _   table) -> table
                            Just (Memo_Divide     _ _   table) -> table
                            Just (Memo_Variable   _     table) -> table
                            Just (Memo_Constant   _     table) -> table

updateMemoTree :: MemoTree -> Att res -> Maybe res -> MemoTree
updateMemoTree(Memo_Root       l     table) attr res = Memo_Root       l     (updateAttr attr res table)
updateMemoTree(Memo_Let        l r   table) attr res = Memo_Let        l r   (updateAttr attr res table)
updateMemoTree(Memo_In         l     table) attr res = Memo_In         l     (updateAttr attr res table)
updateMemoTree(Memo_ConsLet    l r t table) attr res = Memo_ConsLet    l r t (updateAttr attr res table)
updateMemoTree(Memo_ConsAssign l r t table) attr res = Memo_ConsAssign l r t (updateAttr attr res table)
updateMemoTree(Memo_EmptyList        table) attr res = Memo_EmptyList        (updateAttr attr res table)
updateMemoTree(Memo_Plus       l r   table) attr res = Memo_Plus       l r   (updateAttr attr res table)
updateMemoTree(Memo_Minus      l r   table) attr res = Memo_Minus      l r   (updateAttr attr res table)
updateMemoTree(Memo_Time       l r   table) attr res = Memo_Time       l r   (updateAttr attr res table)
updateMemoTree(Memo_Divide     l r   table) attr res = Memo_Divide     l r   (updateAttr attr res table)
updateMemoTree(Memo_Variable   l     table) attr res = Memo_Variable   l     (updateAttr attr res table)
updateMemoTree(Memo_Constant   l     table) attr res = Memo_Constant   l     (updateAttr attr res table)

count m n = case left m of
                     Nothing  -> n
                     Just m'  -> count m' (n+1)

buildMemoTree :: Let.Root -> MemoTree
buildMemoTree (Let.Root l) = Memo_Root (buildMemoTree_Let l) emptyMemoTable
buildMemoTree_Let (Let.Let list i) = Memo_Let (buildMemoTree_List list) (buildMemoTree_In i) emptyMemoTable
buildMemoTree_In (Let.In a) = Memo_In (buildMemoTree_A a) emptyMemoTable
buildMemoTree_List (Let.ConsLet    s l list) = Memo_ConsLet    s (buildMemoTree_Let l) (buildMemoTree_List list) emptyMemoTable
buildMemoTree_List (Let.ConsAssign s a list) = Memo_ConsAssign s (buildMemoTree_A a)   (buildMemoTree_List list) emptyMemoTable
buildMemoTree_List Let.EmptyList             = Memo_EmptyList emptyMemoTable
buildMemoTree_A (Let.Plus     a1 a2) = Memo_Plus     (buildMemoTree_A a1) (buildMemoTree_A a2) emptyMemoTable
buildMemoTree_A (Let.Minus    a1 a2) = Memo_Minus    (buildMemoTree_A a1) (buildMemoTree_A a2) emptyMemoTable
buildMemoTree_A (Let.Time     a1 a2) = Memo_Time     (buildMemoTree_A a1) (buildMemoTree_A a2) emptyMemoTable
buildMemoTree_A (Let.Divide   a1 a2) = Memo_Divide   (buildMemoTree_A a1) (buildMemoTree_A a2) emptyMemoTable
buildMemoTree_A (Let.Variable s)     = Memo_Variable s emptyMemoTable
buildMemoTree_A (Let.Constant i)     = Memo_Constant i emptyMemoTable

lookupAttr :: Att a -> MemoTable -> Maybe a
lookupAttr Attr_algol68          (v,_,_,_,_,_) = v
lookupAttr Attr_errsAlgol        (_,v,_,_,_,_) = v
lookupAttr Attr_calculate        (_,_,v,_,_,_) = v
lookupAttr Attr_upGetVarValue    (_,_,_,v,_,_) = v
lookupAttr Attr_downGetVarValue  (_,_,_,_,v,_) = v
lookupAttr Attr_oneUpGetVarValue (_,_,_,_,_,v) = v

updateAttr :: Att a -> Maybe a -> MemoTable -> MemoTable
updateAttr Attr_algol68          v (_,b,c,d,e,f) = (v,b,c,d,e,f)
updateAttr Attr_errsAlgol        v (a,_,c,d,e,f) = (a,v,c,d,e,f)
updateAttr Attr_calculate        v (a,b,_,d,e,f) = (a,b,v,d,e,f)
updateAttr Attr_upGetVarValue    v (a,b,c,_,e,f) = (a,b,c,v,e,f)
updateAttr Attr_downGetVarValue  v (a,b,c,d,_,f) = (a,b,c,d,v,f)
updateAttr Attr_oneUpGetVarValue v (a,b,c,d,e,_) = (a,b,c,d,e,v)

data ConstructorMemo = C_Memo_Root      
                     | C_Memo_Let       
                     | C_Memo_In        
                     | C_Memo_ConsLet   
                     | C_Memo_ConsAssign
                     | C_Memo_EmptyList 
                     | C_Memo_Plus      
                     | C_Memo_Minus     
                     | C_Memo_Time      
                     | C_Memo_Divide    
                     | C_Memo_Variable  
                     | C_Memo_Constant

constructorMemo :: MemoAG ConstructorMemo
constructorMemo = do ag <- get
                     case (getHole ag :: Maybe MemoTree) of
                       Just (Memo_Root       _     _) -> return C_Memo_Root      
                       Just (Memo_Let        _ _   _) -> return C_Memo_Let       
                       Just (Memo_In         _     _) -> return C_Memo_In        
                       Just (Memo_ConsLet    _ _ _ _) -> return C_Memo_ConsLet   
                       Just (Memo_ConsAssign _ _ _ _) -> return C_Memo_ConsAssign
                       Just (Memo_EmptyList        _) -> return C_Memo_EmptyList 
                       Just (Memo_Plus       _ _   _) -> return C_Memo_Plus      
                       Just (Memo_Minus      _ _   _) -> return C_Memo_Minus     
                       Just (Memo_Time       _ _   _) -> return C_Memo_Time      
                       Just (Memo_Divide     _ _   _) -> return C_Memo_Divide    
                       Just (Memo_Variable   _     _) -> return C_Memo_Variable  
                       Just (Memo_Constant   _     _) -> return C_Memo_Constant  
                       _                              -> error  "Error on constructor!"

lexemeString :: Zipper MemoTree -> String
lexemeString ag = case (getHole ag :: Maybe MemoTree) of
                     Just (Memo_Variable x  _) -> x
                     _                 -> error "Error on lexemeString"

lexemeInt :: Zipper MemoTree -> Int
lexemeInt ag = case (getHole ag :: Maybe MemoTree) of
               Just (Memo_Constant x  _) -> x
               _                 -> error "Error on lexemeInt"

lexemmeConsLet :: Zipper MemoTree -> String
lexemmeConsLet ag = case (getHole ag :: Maybe MemoTree) of
                     Just (Memo_ConsLet x _ _ _) -> x
                     _                 -> error "Error on lexemmeConsLet"

lexemeConsAssign :: Zipper MemoTree -> String
lexemeConsAssign ag = case (getHole ag :: Maybe MemoTree) of
               Just (Memo_ConsAssign x _ _ _) -> x
               _                 -> error "Error on lexemeConsAssign"


memo :: Typeable res => Att res -> MemoAG res -> Dir -> MemoAG res
memo attr eval dir = do
  memo_ag <- get

  let memo_ag' = case dir of 
                       Parent        -> parent memo_ag
                       Child n       -> memo_ag .$ n 
                       Local         -> memo_ag
                       Child_Left  n -> memo_ag .$< n
                       Child_Right n -> memo_ag .$> n
  put memo_ag'
  
  res <- case lookupAttr attr (lexemeMemoTable memo_ag') of
           Just v -> return v
           Nothing  -> do val   <- eval
                          memo_ag'' <-get 
                          let  subtree       = fromJust (getHole memo_ag'' :: Maybe MemoTree)
                          let  new_memo_tree = updateMemoTree subtree attr (Just val)
                          modify $ setHole new_memo_tree
                          return val

  memo_out <- get
  let memo_out' = case dir of
                       Parent        -> let n = count memo_ag 1
                                        in  memo_out .$ n
                       Child      n  -> parent memo_out 
                       Local         -> memo_out
                       Child_Left n  -> memo_out.$>n
                       Child_Right n -> memo_out.$<n
  put memo_out'
  
  return res

-- Attrs
algol68Memo :: Dir -> MemoAG Algol.Its
algol68Memo =  LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_algol68 $ do
    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
    case constr of
          C_Memo_Root       -> algol68Memo (Child 1)
          C_Memo_Let        -> do t1 <- algol68Memo (Child 1)
                                  t2 <- algol68Memo (Child 2)
                                  return (concatIts t1 t2)
          C_Memo_In         -> algol68Memo (Child 1)
          C_Memo_ConsAssign -> do ag <- get
	                          let string = lexemeConsAssign ag
                                  t1 <- algol68Memo (Child 2)
                                  t2 <- algol68Memo (Child 3)
                                  return (concatIts (Algol.ConsIts (Algol.Decl string) t1) t2)
          C_Memo_ConsLet    -> do ag <- get
	                          let string = lexemmeConsLet ag
                                  t1 <- algol68Memo (Child 2)
                                  t2 <- algol68Memo (Child 3)
                                  return (Algol.ConsIts (Algol.Decl string) (Algol.ConsIts (Algol.Block t1) t2))
          C_Memo_EmptyList  -> return Algol.NilIts
          C_Memo_Plus       -> do t1 <- algol68Memo (Child 1)
                                  t2 <- algol68Memo (Child 2)
                                  return (concatIts t1 t2)
          C_Memo_Divide     -> do t1 <- algol68Memo (Child 1)
                                  t2 <- algol68Memo (Child 2)
                                  return (concatIts t1 t2)
          C_Memo_Minus      -> do t1 <- algol68Memo (Child 1)
                                  t2 <- algol68Memo (Child 2)
                                  return (concatIts t1 t2)
          C_Memo_Time       -> do t1 <- algol68Memo (Child 1)
                                  t2 <- algol68Memo (Child 2)
                                  return (concatIts t1 t2)
          C_Memo_Variable   -> do ag <- get
	                          let const = lexemeString ag
                                  return (Algol.ConsIts (Algol.Use const) Algol.NilIts )
          C_Memo_Constant   -> return Algol.NilIts


errsAlgolMemo :: Dir -> MemoAG [String]
errsAlgolMemo = LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_errsAlgol $ do
    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
    case constr of
      C_Memo_Root -> do alg <- algol68Memo Local
                        return $ AlgolSem_mBIn_Rec_Memo.semantics (Algol.Root (alg))

---- Calculate the result of evaluating Let ----
--calculateMemo :: Dir -> MemoAG Int
--calculateMemo = LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_calculate $ do
--    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
--    case constr of
--      C_Memo_Root     -> do errs <- errsAlgolMemo Local
--                            if (length errs /= 0)
--                            then return 0 -- error $ "Errors found -> " ++ (show errs)
--                            else calculateMemo (Child 1)
--      C_Memo_Let      -> calculateMemo (Child 2)
--      C_Memo_In       -> calculateMemo (Child 1)
--      C_Memo_Plus     -> do n1 <- calculateMemo (Child 1)
--                            n2 <- calculateMemo (Child 2)
--                            return $ (n1 + n2)
--      C_Memo_Divide   -> do n1 <- calculateMemo (Child 1)
--                            n2 <- calculateMemo (Child 2)
--                            return $ (div n1 n2)
--      C_Memo_Minus    -> do n1 <- calculateMemo (Child 1)
--                            n2 <- calculateMemo (Child 2)
--                            return $ (n1 - n2)
--      C_Memo_Time     -> do n1 <- calculateMemo (Child 1)
--                            n2 <- calculateMemo (Child 2)
--                            return $ (n1 * n2)
--      C_Memo_Variable -> do ag <- get
--                            upGetVarValueMemo (lexemeString ag) Local
--      C_Memo_Constant -> do ag <- get
--                            return $ (lexemeInt ag)
--
--upGetVarValueMemo :: String -> Dir -> MemoAG Int
--upGetVarValueMemo str = LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_upGetVarValue $ do
--    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
--    case constr of
--      C_Memo_Root -> downGetVarValueMemo str (Child 1)
--      C_Memo_Let  -> downGetVarValueMemo str Local
--      _           -> upGetVarValueMemo   str Parent
--
--downGetVarValueMemo :: String -> Dir -> MemoAG Int
--downGetVarValueMemo str = LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_downGetVarValue $ do
--    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
--    case constr of
--      C_Memo_Let        -> downGetVarValueMemo str (Child 1)
--      C_Memo_ConsAssign -> do ag <- get
--                              if   (lexemeConsAssign ag == str)
--                              then calculateMemo (Child 2)
--                              else downGetVarValueMemo str (Child 3)
--      C_Memo_ConsLet    -> do ag <- get
--                              if   (lexemmeConsLet ag == str)
--                              then calculateMemo (Child 2)
--                              else downGetVarValueMemo str (Child 3)
--      C_Memo_EmptyList  -> oneUpGetVarValueMemo str Local
--
--oneUpGetVarValueMemo :: String -> Dir -> MemoAG Int
--oneUpGetVarValueMemo str = LetSem_Algol68_mBIn_Rec_FullMemo.memo Attr_downGetVarValue $ do
--    constr <- LetSem_Algol68_mBIn_Rec_FullMemo.constructorMemo
--    case constr of
--      C_Memo_Let -> upGetVarValueMemo str Parent
--      _          -> oneUpGetVarValueMemo str Parent


--semanticsMemo :: Let.Root -> Int
--semanticsMemo t = let ag = toZipper $ LetSem_Algol68_mBIn_Rec_FullMemo.buildMemoTree t
--                  in  fst $ runState (calculateMemo Local) ag

nameAnalysisMemo :: Let.Root -> [String]
nameAnalysisMemo t = let ag = toZipper $ LetSem_Algol68_mBIn_Rec_FullMemo.buildMemoTree t
                     in  fst $ runState (errsAlgolMemo Local) ag

--evaluatorMemo :: Let.Root -> (Int, [String])
--evaluatorMemo t = let l = semanticsMemo t
--                      r = nameAnalysisMemo t
--                  in  (l,r)











