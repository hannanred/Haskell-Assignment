-----------------------------------------------------------------------------
-- CS 300 Spring 2022 Assignment 1 Part 2
-- Deadline: Monday 14th February 2022
-- 
-- Please press alt Z to enable Word Wrap
-- 
-- Guidelines:  
-- Feel free to make any helper functions where ever you need them
-- Do not show your code or make it accessible to anyone other than the course staff
-- The arguments and return types of the functions should not be changed
-- You cannot use built-in functions 
-- For queries related to the trees part of the assignment, please contact Ahmed, Sameer or Dawar.
-- For queries relared to the Matrix part of the assignment, please contact Huzaifa.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Trees
-----------------------------------------------------------------------------

data Tree a = Nil | Node (Tree a) (Tree a) a

--Following functions will allow you to print a binary tree:

helpPrint:: Int -> String
helpPrint= \arg -> 
    case arg of
        x | x==0 -> ""
        _ -> "\n" ++ concat (replicate (arg-1) "|   ") ++ "+---"
 

showTree :: (Show a) => Tree a -> Int -> String
showTree = \t -> \ind ->
    case t of
        (Node l r val) ->   
            (helpPrint ind) ++ (show val) ++ (showTree l (ind+1)) ++ (showTree r (ind+1)) 
        _ -> ""

       
instance Show a => Show (Tree a) where
    show = \t ->
        showTree t 0 



-- -- Question 1 : Represent value of Pi upto 5 dp with a binary tree and store it in the variable 'btPi'. Pi=3.14159

-- Hint:
-- Printing the tree:
-- Node (Node (Node (Node Nil (Node Nil Nil 9) 3) (Node Nil Nil 4) 6) Nil 8) (Node Nil Nil 2) 1
-- will output:
-- 1
-- +---8
-- |   +---6
-- |   |   +---3
-- |   |   |   +---9
-- |   |   +---4
-- +---2
btPi = Node (Node (Node (Node Nil Nil 3) Nil 1) Nil 4) (Node Nil (Node Nil Nil 9) 5) 1

--main = print(btPi)

-- -- Part b : Represent value of Euler's number (e) upto 5 dp with a binary tree and store it in the variable 'btE'.
-- e = 2.71828

btE = Node (Node (Node Nil Nil 2) Nil 7)  (Node Nil (Node Nil (Node Nil Nil 8) 2) 8) 1

--main = print(btE)




-- -- Question 2 : Traverse your binary tree of Pi and euler's number using Preorder, Inorder or Postorder traversal (ONLY 1 traversal) and store it in a list. Please state the traversal you will be using:
-- Traversal used: <Inorder Traversal>  

btTraversal :: Tree a -> [a]
btTraversal Nil = []
btTraversal (Node left right a) =  btTraversal left ++ [a] ++ btTraversal right -- left root right


-- main = print(btTraversal btPi)
-- Expected output: [3,1,4,1,5,9]
-- main = print(btTraversal btE)
-- Expected output: [2,7,1,8,2,8]




-- -- Question 3:
-- For this question, you are given with 2 arguments, a binary tree and an integer n. Write a program that returns the list of paths that sum to the integer n. Please note that a path has to be from the root of the tree to a leaf node.
-- Example: 

-- Value of n = 15 

-- Tree:
-- 6
-- +---4        
-- |   +---2    
-- |   |   +---1
-- |   |   +---3
-- |   +---5    
-- +---8        
-- |   +---7    
-- |   +---16 

-- output: [[6,4,2,3],[6,4,5]]

countPaths :: Tree Int -> Int -> [[Int]]
countPaths tree n = helper2 (listOfPaths tree) n

helper1 (x:xs) = x + helper1 xs
helper1 [] = 0  --sum all elements in a path

helper2 (x:xs) n = if (helper1 x) == n
                    then [x] ++ helper2 xs n
                    else helper2 xs n  -- this function is used to check if different paths have sum equal to n
helper2 [] n = []

listOfPaths (Node Nil Nil a) = [[a]]
listOfPaths Nil = []
listOfPaths (Node left right a) = joiner a (listOfPaths left ++ listOfPaths right)  -- Gives the list of all paths

-- joiner to append root element
joiner r (x:xs) = [r:x] ++ joiner r xs   -- Helper which is Basically an implementation of the map function, this is used to append the root element for the list of paths
joiner r [] = []



-- main = print(countPaths testT 15)

-- -- Question 4:
-- For this question, you are given with 2 arguments, a binary tree and an integer n. Each node of the tree is a tuple which has a unique key (which will be an integer) and a switch which can be true and false. You will pass the ball from the root of the tree until it reaches a point from which it can't go forward. At a node, if its switch is off (false) then the ball will go to its left, otherwise to the right. If there is no path forward then the ball stops and the next iteration starts. After the ball is passed, the switch changes.. for example if a switch is off and it passes the ball to its left or right, then it will turn on . You will repeat this process n times and will return the node which contains the ball at the nth iteration. Find a simple visualization here: https://drive.google.com/file/d/1aGlUdtHhpKkX0nuTxfU2_5-_yF9Cj8TP/view?usp=sharing
-- Example: 

-- value of integer n = 2

-- Tree:
-- (6,True)
-- +---(5,False)    
-- |   +---(1,True) 
-- |   +---(3,False)
-- +---(2,False) 

-- Output: 1


--commented out because integer int bool error asked TA for it
--droppingBall :: Tree (Int, Bool) -> Int -> Int 
droppingBall (Node left right (k,v)) n = element(pathStop (lastTree (Node left right (k,v)) n))


lastTree (Node left right (k,v)) n = if (n/= 1) then 
                                        lastTree (pathReverser (Node left right (k,v)) (droppingBallHelper (Node left right (k,v)))) (n-1)
                                        else droppingBallHelper (Node left right (k,v))  --returns the last path taken by the ball

pathStop (x:xs) = if xs == [] then x else pathStop xs --returns the last node value from the list of nodes accessed by ball at an iteration


tree1 = (Node (Node (Node Nil Nil (1,True)) (Node Nil Nil (3,False)) (5,False)) (Node Nil Nil (2,False)) (6,True))
element (k,v) = k 

droppingBallHelper Nil = []
droppingBallHelper (Node left right (k,v)) = if (v == True) && (checker right /= Nothing)
                                                then (k,v) :droppingBallHelper right
                                                else if (v==False) && (checker left /= Nothing)
                                                    then (k,v):droppingBallHelper left
                                                    else (k,v):[]
                                                 --PATH BALL TAKES AT A GIVEN ITERATION
 


pathReverserHelper (Node left right (k,v)) = (Node left right (switchReversal (k,v))) --HELPER TO REVERSE SWITCH

pathReverser (Node left right (k,v)) (x:xs) = if (k,v) == x then pathReverserHelper (Node (pathReverser left xs) (pathReverser right xs) (k,v)) else (Node (pathReverser left xs) (pathReverser right xs) (k,v))
pathReverser (Node left right (k,v)) [] = (Node left right (k,v))
pathReverser Nil _ = Nil  --REVERSES THE PATH TAKEN IN TREE AND RETURNS TREE

checker Nil = Nothing
checker (Node left right (k,v)) =Just (k,v) -- HELPER checks if a tree is empty or not


switchReversal (k,v) = if v == True
                        then (k,False)
                        else (k,True)  -- HELPER reverses the value at the given node




-- -- Question 5: 
-- For this Question, you will be given with a postfix expression in the form of a string. You have to construct an expression tree from this string. After constructing the expression tree you will have to use it to convert it into infix.
-- Example: 
-- Postfix: a b + c d e + * *
-- infix: (a+b)*(c*(d+e))

-- IT IS ASSUMED THAT EXPRESSION IS IN THE FORM "a b + c d e + * *"
-- Expression Tree:

-- '*'
-- +---'+'        
-- |   +---'a'    
-- |   +---'b'    
-- +---'*'        
-- |   +---'C'    
-- |   +---'+'    
-- |   |   +---'d'
-- |   |   +---'e'


constructExpressionTree :: String -> Tree String 
constructExpressionTree postFixExpression = constructExpressionTreeHelper postFixExpression -- TO DO

constructExpressionTreeHelper postFixExpression = (treeMaker (listOfCharacters postFixExpression) [])!!0

appendNode node list = list ++ [node]
--A list and a list of nodes
treeMaker (x:xs) nodesList = if x /= ' ' && x/= '+' && x/= '-' && x /= '/' && x/='*'
                    then treeMaker xs (appendNode (Node Nil Nil [x]) nodesList)
                    else treeMaker xs (appendNode (Node (secondLastNode nodesList) (lastNode nodesList) [x]) (listWithoutLast2 nodesList (lengthOfList nodesList)))
treeMaker [] nodesList = nodesList


listOfCharacters (x:xs) = if x /= ' ' then x:listOfCharacters xs else listOfCharacters xs
listOfCharacters [] = []



lastNode node = node!!((lengthOfList node)-1)
secondLastNode node = node!!((lengthOfList node)-2)
lengthOfList (x:xs) = 1 + lengthOfList xs 
lengthOfList [] = 0

listWithoutLast2 (x:xs) n = if (n-2) > 0
                                then x:listWithoutLast2(xs) (n-1)
                                else []




postfix = "a b + c d e + * *"

convertToInfix :: Tree String -> String 
convertToInfix postFixTree = removeExtraBrackets (infixMaker (traversalPostOrder(postFixTree)) []) -- TO DO 


addchar char list = list ++ [char]

traversalPostOrder (Node left right a) =  (traversalPostOrder left) ++ (traversalPostOrder right) ++ a
traversalPostOrder Nil = []

infixMaker (x:xs) final = if x /= '+' && x/='-' && x/='*' && x/='/'
                            then infixMaker xs (addchar [x] final)
                            else infixMaker xs (addchar ("(" ++ (secondLastChar final) ++ [x] ++ (lastChar final) ++ ")") (listWithoutLast2 final (lengthOfList final)))
infixMaker [] final = final

removeExtraBrackets list = removeExtraBrackets2((list!!0))
removeExtraBrackets2 (x:xs) = listWithoutLast1 xs (lengthOfList xs)


listWithoutLast1 (x:xs) n = if (n-1) > 0
                                then x:listWithoutLast1(xs) (n-1)
                                else []
listWithoutLast1 [] _ = []

lastChar list = list!!((lengthOfList list)-1)
secondLastChar list = list!!((lengthOfList list)-2)

-- BONUS Question: No extra marks but the first 5 people to solve this get a treat from Ahmed and Sameer.
-- -- Question x:
-- For this question, you are given with a binary tree which may or may not be balanced. Write a program that balances the tree and returns it.  
balanceTree:: Tree Int -> Tree Int
balanceTree = undefined -- TO DO




-----------------------------------------------------------------------------
-- Matrices
-----------------------------------------------------------------------------

-- -- Question 6:
-- For this task, you need to develop a function that performs any given operation element-wise on two given matrices. This operation could be addition, subtraction, element-wise product or any operation that has been given to the function as an argument. You cannot use any built-in functions such as map, zip, or zipWith etc. This task would be easier for you if you first make a function that performs any given operation, element-wise, on two lists e.g. 
-- thisFunction :: (a -> b -> c) -> [a] -> [b] -> [c]
-- thisFunction (+) [1, 2, 3] [4, 5, 6] returns [5,7,9]. 
-- However, there is no compulsion to make such a helper function but the same function might be helpful to use in the next part (nxn matrix multiplication) as well.

matrixElemOp :: Num a => [[a]] -> [[a]] -> (a -> a -> a) -> [[a]]
matrixElemOp matrix1 matrix2 op= matrixElemOpHelper matrix1 matrix2 op


m1 = [[4,8],[3,7]]
m2 = [[1,0],[5,2]]

matrixElemOpHelper (x:xs) (y:ys) op = helperMatrix x y op : matrixElemOpHelper xs ys op
matrixElemOpHelper [] [] op = []

helperMatrix (x:xs) (y:ys) op = (x `op` y): helperMatrix xs ys op

-- helperMatrix (op) (x:xs) (y:ys) = (x (op) y): helperMatrix xs ys op
helperMatrix [] [] op = []
-- -- Question 7:
-- In this part, you are going to make a functional matrix product function which can multiply two matrices. These matrices can be of any dimensions, so long as the number of columns of the first matrix is equal to the number of rows of the second matrix (you may assume that user would not enter incompatible matrices). You do not have to worry about the efficiency of your function. The following link might be useful: https://en.wikipedia.org/wiki/Matrix_multiplication#Definition. 

matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMult m1 m2 = multiplier m1 (listOfColumns m2 0) -- TODO


multiplier (x:xs) (y:ys) = multiplierHelper2 x (y:ys) : multiplier xs (y:ys)
multiplier [] (y:ys) = [] -- multiplies each row with all the columns and returns the matrix (using helperfunctions)

multiplierHelper2 x (y:ys) = multiplierHelper x (y) : multiplierHelper2 x ys
multiplierHelper2 x [] = [] --multiplies a given row with all the columns and returns a list

multiplierHelper (x:xs) (y:ys) = (x * y) + multiplierHelper xs ys 
multiplierHelper [] [] = 0                 -- for e.g a row is 2 2 and a column is 1 1 it returns (2x1) + (2x1)          

                               


listOfColumnsHelper (x:xs) n = (x!!n):listOfColumnsHelper xs n
listOfColumnsHelper [] n = []  -- Makes list of the column values of a matrix 
                                
listOfColumns (x:xs) n = if n < (lengthFinder(x))
                            then (listOfColumnsHelper (x:xs) n):listOfColumns (x:xs) (n+1)
                            else [] --helps iterate over different columns and returns a 2d list of all the columns
listOfColumns [] n = []


lengthFinder (x:xs) = 1 + lengthFinder xs  --returns length of a list
lengthFinder [] = 0 
-----------------------------------------------------------------------------
-- The End :}
-----------------------------------------------------------------------------




