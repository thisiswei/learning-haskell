-- 1.1 determine if a string has all unique characters

uniq "" = True
uniq (xs:"") = True
uniq (x:xs) = if elem x xs then False else uniq xs


-- 1.2 write code to reverse a C-style string
-- wtf is C-style string?!

cStyle x = reverse x

-- 1.3 remove duplicate characters in a string

remove "" = ""
remove (x:xs) = if elem x xs then remove xs else (x : remove xs)

-- 1.4 decide if two strings are anagrams or not

anagrams x y = length x == length y && everyElem x y
    where everyElem "" _ = True
          everyElem (x:xs) y = elem x y && everyElem xs y

-- 1.5 write a method to replace all space in string with '%20'

replace "" = ""
replace (x:xs) = if x == ' ' then "%20" ++ (replace xs) else x : replace(xs)

-- 1.6 Give an image represented by an N*N matrix, where each pixel in the
-- image is 4 bytes, write a method to rotate the image by 90 degrees.


-- 1.7 write a algorithm such that if an element in a M*N matrix is 0, its 
-- entire row and column is set to 0



