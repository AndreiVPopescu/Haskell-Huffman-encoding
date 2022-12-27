--Andrei Victor Popescu apopes18@student.aau.dk

import Data.Ord
import Data.List

-- Basic Tree structure
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show


-- Main functions: compress and decompress


-- Takes an input string and returns the compressed code
-- Usage: compress <String>
compress :: String -> String
compress inpt = convertToCode inpt (makeKey inpt)

-- Takes an input string of a compressed text and the list of unique decompression keys and returns the decompressed text
-- Usage: decompress <String> [(<String>,<String>)]

-- Tested using decompress x (makeKey x) where x is a String
decompress :: String -> [(String,String)] -> String
decompress [] _ = ""
decompress encoded code = buildString encoded (convertCodeToTree code (Leaf ""))


-- Compression section


-- makeKey takes a String from input and returns a list of unique characters and their Hoffman code
-- makeKey is used as a wrapper, to shorten the function calls needed to be written in one line and used for debugging and inspection
-- Usage: makeKey <String>
makeKey :: String -> [(String,String)]
makeKey inp = linearizeTree (buildTree (convertToTree (countChars inp []) [])) ""

-- Adds a unique character to a list of unique characters or increments the count of an existing character. Used by countChars
addToList :: String -> [(String,Int)] -> [(String,Int)]
addToList key [] = [(key,1)]
addToList key (head:tail) = if key == (fst head) then [(key, (snd head +1))] ++ tail
        else [head] ++ (addToList key tail)

-- countChars takes an input string and returns a list of occurences for each unique character        
countChars :: String -> [(String,Int)] -> [(String,Int)]
countChars [] count = count
countChars inp count = countChars (tail inp) (addToList (take 1 inp) count)

-- sortTree sorts a list of trees. We use this function instead of one that finds the smallest 2 trees to aggregate.
-- This function ensures the smallest trees are the first in the list.
sortTree :: [Tree (String,Int)] -> [Tree (String,Int)]
sortTree val = sortBy (comparing nodeInt) val

-- Converts a list of unique characters and their occurences into a list of trees ready to be built.
convertToTree :: [(String,Int)] -> [Tree (String,Int)] -> [Tree (String,Int)]
convertToTree [] tree = (sortTree tree)
convertToTree ((key, val):tail) tree = convertToTree tail (tree ++ [Leaf (key,val)])

-- buildNode concatenates the string of 2 nodes and sums their value. Used by buildTree
buildNode :: Tree (String,Int) -> Tree (String,Int) -> (String,Int)
buildNode node1 node2 = ((nodeStr node1 ++ nodeStr node2),(nodeInt node1 + nodeInt node2))

-- buildTree Builds a single Hoffman Tree from a list of trees (starting as leaves)
-- The Nodes retain the concatenated string of the children and the sum for easier debugging and inspection.
buildTree :: [Tree (String,Int)] -> Tree(String,Int)
buildTree [] = Leaf ("",0)
buildTree (head:[]) = Leaf ((nodeStr head),(nodeInt head))
buildTree (head:next:[]) = Node (buildNode head next) head next
buildTree (head:next:tail) = buildTree (sortTree ([Node (buildNode head next) head next] ++ tail))

-- returns the String held by a Node, used by buildNode
nodeStr :: Tree (String, Int) -> String
nodeStr (Leaf x) = fst x
nodeStr (Node (st,val) lft rgth) = st

-- returns the In held by a Node, used by buildNode
nodeInt :: Tree (String,Int) -> Int
nodeInt (Leaf x) = snd x
nodeInt (Node (st,val) lft rgth) = val

-- Returns a list of unique characters with their Hoffman code from a Hoffman Tree.
-- It parses through the Tree, adding a 0 or a 1 in the code string whether the function goes to the left or right child of a tree.
-- Once it reaches a Leaf, it adds the string held by the leaf and the code to reach it to a list of codes for each character. 
linearizeTree :: Tree (String,Int) -> String -> [(String, String)]
linearizeTree (Node (st,val) x y) code = linearizeTree x (code ++ "0") ++ linearizeTree y (code ++ "1")
linearizeTree (Leaf x) code = if code == "" then [(fst x,"0")] else [(fst x, code)]

-- Converts the input string to the compressed code using a given list of keys
convertToCode :: String -> [(String,String)] -> String
convertToCode [] _ = ""
convertToCode a code = (getCode (take 1 a) code) ++ convertToCode (tail a) code

-- Finds and returns the Hoffman code for a particular character. Used by convertToCode
getCode :: String -> [(String,String)] -> String
getCode letter (head:tail) = if letter == (fst head) then (snd head) else getCode letter tail


-- Decompression section


-- Takes a list of Hoffman keys and returns the Hoffman Tree
convertCodeToTree :: [(String,String)] -> Tree(String) -> Tree(String)
convertCodeToTree (head:tail) tree = convertCodeToTree tail (insertToTree head tree)
convertCodeToTree [] tree = tree

-- Adds a pair of letter, Hoffman code and adds it to the Hoffman Tree as a new Leaf.
-- The function goes through the Code and explores the tree to the left or right depending on the current character of the code.
-- Once it reaches the end of the code, it creates a new Leaf with the character.
insertToTree :: (String,String) -> Tree (String) -> Tree (String)
insertToTree (val, path) (Node _ lft rght) = case (take 1 path) of
        "0" -> Node "" (insertToTree (val, (tail path)) lft) rght 
        "1" -> Node "" lft (insertToTree (val, (tail path)) rght)
        _ -> Leaf (take 1 val)
insertToTree (val, path) (Leaf t) = case (take 1 path) of
        "0" -> Node "" (insertToTree (val, (tail path)) (Leaf "")) (Leaf "") 
        "1" -> Node "" (Leaf "") (insertToTree (val, (tail path)) (Leaf ""))
        _ -> Leaf (take 1 val)

-- Parses through a Hoffman Tree by sequentially reading the compressed code, going through the left or right child depending on the next character of the code.
-- Once it reaches a Leaf, it adds the character held by the leaf to the output string and starts again from the root of the Hoffman Tree.
-- It exits once the string containing the compressed code is empty.
getStr :: String -> Tree(String) -> Tree (String) -> String
getStr inp (Leaf v) root = v ++ (getStr inp root root)
getStr (x:tail) (Node _ lft rght) root = if x == '0' then getStr tail lft root else getStr tail rght root
getStr [] _ _ = []

-- Wrapper used by decompress for getStr to not have to create the HoffmanTree twice. 
buildString :: String -> Tree(String)-> String
buildString str tree = getStr str tree tree 