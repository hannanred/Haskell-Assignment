-----------------------------------------------------------------------------
-- Counting Sort
-- CS 300 Spring 2022 Assignment 1 Part 1
-- Deadline: Monday, 7th February 2022
-- 
-- Please press alt Z to enable Word Wrap
-- 
-- Guidelines:  
-- The assignment is based on counting sort algorithm
-- Feel free to make any helper functions where ever you need them
-- Do not show your code or make it accessible to anyone other than the course staff
-- The arguments and return types of the functions should not be changed
-----------------------------------------------------------------------------


--take_head and take_tail were self-defined head tail functions 
take_head (x:xs) = x


take_tail (x:xs) = xs

--other helper functions were written alongside the questions according to need


-- Question 1: CountOccurance function take in 2 arguments, an integer and a list, and returns the number of times the integer exists in the list.
countOccurance :: Int -> [Int] -> Int
countOccurance number l = numOfElements number l    
numOfElements n (x:xs) = if x == n
                            then 1 + numOfElements n xs    --increment if number is at the head of the list and pass the tail of the list
                            else 0 + numOfElements n xs    --do not increment but pass the tail of the list
numOfElements n [] = 0

--countOccurance number l = length [x | x<-l, x==number]


-- Question 2: The maxOf function returns the maximum integer present in the list.
maxOf :: Ord a => [a] -> a
maxOf (x:xs:xss) = if x > xs then maxOf(x:xss) else maxOf(xs:xss)
maxOf [x] = x  --stop at the last element
-- this function will recursively call maxOf until only one element is left, this element will be the largest due to the if condition which compares the head element to the second element
-- ofcourse, after the comparison the function is called again, if head is bigger than the list without second element if second element is bigger than the tail of the list (without the head) is used to call the function


-- Question 3: The indexArray function takes an integer and returns a list of integers from 0 till that integer (inclusive)
indexArray :: Int -> [Int]
indexArray number = indexArrayGenerator number 0 --start from 0
indexArrayGenerator number n = if (n /= number)  --until we reach the number till which we want to create the list for 
                                  then (n): indexArrayGenerator number (n+1)  -- start from 0, increment till we reach the number and keep calling the function to create the list
                                  else number:[] --when number reached it means we end the list first by using the number we were aiming for and then [] to signify end of the list

-- Question 4: The makeCountArray function takes in 2 lists, array1 and array2 where array1 can be a list with any numbers and array2 will specifically be an array consisting elements from 0 till maximum of array1 in ascending order. For now you can safely assume that array2 will be always correct and according to the format
-- example array1 = [0,1,2,1,1,2,3] array2 = [0,1,2,3] --> return [1,3,2,1] -- explanation: 0 exists 1 time, 1 exists 3 times and so on..
makeCountArray :: [Int] -> [Int] -> [Int]

makeCountArray array1 array2 = if array2 /= [] --if array2 is not an empty list
                                    then countOccurance (take_head array2) array1: makeCountArray array1 (take_tail array2)  --find the count of head of array2 in array1 and recursively call the function with the tail of array2
                                    else [] --end list



-- Question 5: makePairs takes in 2 integer lists and returns a list of tuples by combining both of the arrays. 
-- Example [1,2,3,4] and [4,3,2,1] -> returns [(1,4),(2,3), (3,2), (4,1)] 
makePairs :: [Int] -> [Int] -> [] (Int,Int)
makePairs list1 list2 = if list1 /=[] && list2 /=[]  --it is assumed both lists will be of equal length
                            then ((take_head list1),(take_head list2)) : makePairs (take_tail list1) (take_tail list2)
                            else [] 

-- Question 6: CountingSort function takes in an array and uses the above functions to sort the array using counting sort technique. Feel free to use any helper functions according to your needs.
countingSort :: [Int] -> [Int]
countingSort inputarr = sorter (helper1 inputarr)
--helper1 list n = ((take_head list)+n): helper1 (take_tail list) ((take_head list)+n) --cumulative addition in the array by helper 1
--helper1 list n = if (list/= [])
  --                  then ((take_head list)+n): helper1 (take_tail list) ((take_head list)+n)
    --                else []


--helper2 list = 0 : (removeLast list)--rotate clockwise once
--removeLast list = if ((take_tail list) /= [])
       --             then (take_head list) : removeLast (take_tail list) -- returns a list without the last element
         --           else []   

helper1 inputarr1= makePairs  (indexArray (maxOf inputarr1)) (makeCountArray inputarr1 (indexArray (maxOf inputarr1))) --makes a pair of index array and count array

sorter z = if (z /= []) --this function uses the tuple list, takes the frequency and index and recursively calls inserter on the tail of the tuple list
            then inserter (fst (take_head z)) (snd (take_head z)) ++ sorter (take_tail z) --concotate lists 
            else [] 

inserter number count = if (count /= 0)
                         then number : inserter number (count-1) --creates a list depending on the count of the specific element, i.e a list of 2 with count 3 will be [2,3,3]
                         else []


-----------------------------------------------------------------------------
-- The End :)
-----------------------------------------------------------------------------





