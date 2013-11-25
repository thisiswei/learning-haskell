{- You are in a grid of city blocks,
a nd you want to know how many different paths there are between your current corner and a destination corner
(without taking any steps in the wrong direction). 
The current corner is given by two non-negative integers x, y 
indicating the number of blocks east and north you are,
respectively, of the destination (in other words the desitination is at 0,0).
 For example, if you start at (1, 1) then there are 2 paths:
you could go west one block and then south,
or south one block and then west.
If you start at (2, 1) there are three paths: [west, west, south], [west, south, west], 
and [south, west, west]. Write num_paths(x, y) that returns the number of paths from location x,y to the destination 0,0. 
(You don't need to say what the individual paths are, just count them.) -}

num_paths _ 0 = 1
num_paths 0 _ = 1 
num_paths x y = num_paths (x-1) y + num_paths x (y-1)

rpn = head . foldl f [] . words
    where f (x:y:ys) "*" = (x*y) : ys
          f (x:y:ys) "+" = (x+y) : ys
          f (x:y:ys) "-" = (x-y) : ys
          f xs wtf = (read wtf):xs

f (x:y:ys) "*" = (x*y) : ys
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs






