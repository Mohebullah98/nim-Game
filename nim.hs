subtractBoard :: Board -> Int -> Int -> Board
-- subtractBoard only subtracts values from a row if row == index
subtractBoard xs a b = [if i == a then x-b else x | (x,i) <- zip xs [1..(length xs)]]
type Board = [Int]
initial :: Board
initial = [5,4,3,2,1]
stars :: Int -> String
stars n = concat (replicate n "* ")
-- replicate * will give us a list of "* " strings. Concat will break the list into a single string.
printRow :: Int -> Int -> IO ()
printRow r n = putStrLn ((show r) ++ ": " ++ (stars n))
printBoard :: Board -> IO ()
-- printBoard displays each row with amount for that row as stars.
printBoard [a,b,c,d,e]= do printRow 1 a
                           printRow 2 b
                           printRow 3 c
                           printRow 4 d
                           printRow 5 e
-- checkBoard checks if board is all zeroes, so game over
checkBoard :: Board -> Bool
checkBoard board = if sum board == 0 then True else False
-- compareValue checks if the input to subtract is a valid amount
compareValue xs x y = if y > (xs !! (x-1)) then True else False
nim :: IO ()
nim = play initial
play :: Board -> IO ()
play initial =do printBoard initial
                 putStrLn "Next player choose: "
                 putStr "Row: "
                 x <- getLine
                 putStr "Amount: "
                 y <- getLine
                 if compareValue initial (read x :: Int) (read y :: Int) then do
                    putStrLn "Invalid Amount!"
                    play initial
                 else
                     if checkBoard (subtractBoard initial (read x :: Int) (read y :: Int)) then
                        putStrLn " Congratulations, You Won! "
                     else play (subtractBoard initial (read x :: Int) (read y :: Int))
