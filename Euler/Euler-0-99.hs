e01 = print $ sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

main = e01