G52AFP Coursework 1 - Noughts and crosses
   
James Key
psyjpk@nottingham.ac.uk
Joe Staines
psyjs2@nottingham.ac.uk

----------------------------------------------------------------------

For flexibility, the size of the board is defined as a constant:

> import Data.List
> import Data.Char
> import Data.Ord
> size                  :: Int
> size                  =  3

The board itself is represented as a list of list of player values,
with the width and height of the board always being of the above size:

> type Board            =  [[Player]]

-- In turn, a player value is either a nought, a blank, or a cross, with
-- a blank representing a space on the board that is not yet occupied:

> data Player           =  Nought | Blank | Cross
>                          deriving (Ord, Eq, Show)

-- The following code displays a board on the screen:

> showBoard             :: Board -> IO ()
> showBoard             =  putStrLn . unlines . concat
>                             . separate hbar . map showRow
>                          where
>			      hbar = [replicate (size * 6) '-']
>
> showRow               :: [Player] -> [String]
> showRow               =  beside . separate vbar . map showPlayer
>                          where
>                             beside = foldr1 (zipWith (++))
>                             vbar   = replicate (size + 2) "|"
>
> showPlayer            :: Player -> [String]
> showPlayer Nought     =  ["     ", " +-+ ", " | | ", " +-+ ", "     "]
> showPlayer Blank      =  ["     ", "     ", "     ", "     ", "     "]
> showPlayer Cross      =  ["     ", " \\ / ", "  X  ", " / \\ ", "     "]
>
> separate              :: a -> [a] -> [a]
> separate x []         =  []
> separate x [y]        =  [y]
> separate x (y:ys)     =  y : x : separate x ys

----------------------------------------------------------------------

> blankBoard :: Board
> blankBoard = [[Blank, Blank, Blank], [Blank, Blank, Blank], [Blank, Blank, Blank]]

> noughtWinDiag :: Board
> noughtWinDiag = [[Blank, Blank, Nought], [Blank, Nought, Blank], [Nought, Cross, Blank]]

> noughtAlmostDiag :: Board
> noughtAlmostDiag = [[Blank, Blank, Nought], [Cross, Nought, Cross], [Blank, Cross, Nought]]

> noughtWinRow :: Board
> noughtWinRow = [[Nought, Nought, Nought], [Blank, Cross, Blank], [Blank, Cross, Nought]]

> drawBoard :: Board
> drawBoard = [[Nought, Cross, Nought],[Nought, Cross, Cross],[Cross, Nought, Nought]]

> halfWayBoard :: Board
> halfWayBoard = [[Nought, Blank, Blank],[Cross, Cross, Nought],[Cross, Nought, Blank]]

> turn :: Board -> Player
> turn xss = turnAux (playerAmount xss Cross) (playerAmount xss Nought)

> playerAmount :: Board -> Player -> Int
> playerAmount xss pl = length( filter (==pl) (concat xss))

> turnAux :: Int -> Int -> Player
> turnAux x y 
>			| x == y        = Nought
>			| otherwise    	= Cross 

> winState       :: Board -> Bool
> winState b     = 	any checkSame (rows b) ||
>					any checkSame (cols b) ||
>					any checkSame (diags b)

> checkDraw :: Board -> Bool
> checkDraw b
> 		| (blankSpots b == [] && winState b == False) 	= True
> 		| otherwise										= False

> winner :: Board -> Player
> winner b = negatePlayer (turn b)

> negatePlayer :: Player -> Player
> negatePlayer Cross = Nought
> negatePlayer _ = Cross

> rows :: Board -> [[Player]]
> rows = id

> cols :: Board -> [[Player]]
> cols = transpose

> diags :: Board -> [[Player]]
> diags b = [[(b !! x) !! x | x <- [0..(size-1)]], [(b !! x) !! (((length b) - x)-1) | x <- [0..(size-1)]]]

> checkSame             :: [Player] -> Bool
> checkSame (x:xs)	
>						| x == Blank 	= False
> 						| otherwise 	= (and $ map (==x) (xs))


> move :: Board -> Int -> Board
> move b x 
>	| (concat b !! x) == Blank = repl b b x
>	| otherwise = b
	
> repl :: Board -> Board -> Int -> Board
> repl _ [] x = []
> repl st (b:bs) x
>				| x `div` size == 0 = repl2 st b (x `mod` size): bs
>				| otherwise			= b: repl st bs (x - size)

> repl2 :: Board -> [Player] -> Int -> [Player]
> repl2 b [] x = []
> repl2 b (c:cs) x
>				| x == 0 	= turn b : cs
>				| otherwise	= c: repl2 b cs (x - 1)

> dumbAIMove :: Board -> Board
> dumbAIMove b = move b (aiInput $ concat b)

> aiInput :: [Player] -> Int
> aiInput [] = 0
> aiInput (x:xs)
> 			| x == Blank =  0
> 			| otherwise =  1 + aiInput xs

> mainloop :: IO ()
> mainloop = loop blankBoard
> 	where 
> 		loop board = do
> 			showBoard board
>			if (winState board) 
>				then
>					putStrLn  ((show (winner board)) ++  " wins!")
>				else if (checkDraw board)
>					then 
>						putStrLn "It's a draw!"
>					else
>						case (turn board) of 
>							Nought 	-> getAIInput board
>							Cross 	-> getUserInput board
>		getUserInput board = do
>			putStrLn "Where do you want to move (0-8)? "
> 			n <- readLn
>			(loop (move board n))
>		getAIInput board = do
>			putStrLn "AI's turn"
>			(loop (grabBoard (growTreeOne board) (correctBoardIndex board)))

> data Tree a = Node a [Tree a] deriving (Show)

> growTreeAll :: Board -> Tree Board
> growTreeAll b 
>	| (winState b || checkDraw b) 	= Node b []
> 	| otherwise 					= Node b [growTreeAll (move b x) | x <- blankSpots b]

> growTreeOne :: Board -> Tree Board
> growTreeOne b = Node b [Node (move b x) [] | x <- blankSpots b]

> grabBoard :: Tree Board -> Int -> Board
> grabBoard (Node _ xs) i = getB $ xs !! i
>	where
>		getB (Node b _) = b

> blankSpots :: Board -> [Int]
> blankSpots b = [ snd p | p <- (zip (concat b) [0..]),  fst p == Blank]

> treeCount :: Tree a -> Int
> treeCount (Node _ []) = 0
> treeCount (Node _ xs) = 1 + sum (map treeCount xs)



--> treeWinner :: Tree Board -> (Player, Int)
--> treeWinner (Node b []) = (minimax b,0)
--> treeWinner (Node b xs) = head $ filter ((==(turn b)).fst) getAllNodes
-->		where
-->			getAllNodes = (map treeWinner xs)


> treeWinner :: Tree Board -> (Player, Int)
> treeWinner (Node b []) = (minimax b,0)
> treeWinner (Node b xs) 
>		| turn b == Cross = head $ maxTuple $ reZip getAllNodes
>		| turn b == Nought = head $ minTuple $ reZip getAllNodes
>			where
>				getAllNodes = (map treeWinner xs)

> correctBoardIndex :: Board -> Int
> correctBoardIndex = snd . treeWinner . growTreeAll

> reZip :: [(Player,Int)] -> [(Player,Int)]
> reZip xs = zip (fstList xs) [0..]

> fstList :: [(a,b)] -> [a]
> fstList xs = map fst xs


> minimax :: Board -> Player
> minimax b
> 	| winState b = winner b
>	| otherwise = Blank

> minTuple :: [(Player,Int)] -> [(Player,Int)]
> minTuple xs = sortBy (comparing fst) xs

> maxTuple :: [(Player,Int)] -> [(Player,Int)]
> maxTuple = reverse . minTuple

> main =  mainloop

--> play :: Board -> IO Board
--> play b = do
-->			showBoard b
-->			play (ask b)

--> ask :: Board -> IO Board
--> ask b = do
-->			putStr "Where do you want to move (0-8)? "
-->			n <- readLn
-->			return (move b n)

-->			(ord getChar) - 48
