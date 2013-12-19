

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibs2 = 0 : 1 : [ a+b | a <- fibs2 | b <- tail fibs2 ]
fibs3 = 0 : scanl (+) 1 fibs3

exponential_fib 0 = 0
exponential_fib 1 = 1
exponential_fib n = exponential_fib (n-2) + exponential_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

{-  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
	fibs = 0 : 1 : ((+) 0 1 : zipWith (+) (tail fibs) (tail (tail fibs)))
	fibs = 0 : 1 : 1 : ((+) 1 1 : zipWith (+) (tail (tail fibs)) (taii (tail (tail fibs)))))    
	fibs = 0 : 1 : 1 : 2 : ((+) 1 2 : zipWith (+) (tail (tail (tail fibs))) (tail (taii (tail (tail fibs))))))
	fibs = 0 : 1 : 1 : 2 : 3 : ((+) 2 3 : zipWith (+) (tail (tail (tail (tail fibs)))) (tail (tail (taii (tail (tail fibs)))))))
-}