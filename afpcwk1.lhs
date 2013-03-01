G52AFP Coursework 1 - Noughts and crosses
   
James Key
psyjpk@nottingham.ac.uk
Joe Staines
psyjs2@nottingham.ac.uk

----------------------------------------------------------------------

For flexibility, the size of the board is defined as a constant:

> import Data.List
> import Data.Char
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

> noughtWinRow :: Board
> noughtWinRow = [[Nought, Nought, Nought], [Blank, Cross, Blank], [Blank, Cross, Nought]]

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


> move :: Board -> IO Int -> Board
> move b x = repl b x

> repl :: Board -> IO Int -> Board
> repl [] x = []
> repl (b:bs) x
>				| x `div` size == 0 = repl2 (b:bs) b (x `mod` size): repl bs (x - size)
>				| otherwise			= b: repl bs (x - size)

> repl2 :: Board -> [Player] -> Int -> [Player]
> repl2 b [] x = []
> repl2 b (c:cs) x
>				| x == 0 	= turn b : repl2 b cs (x - 1)
>				| otherwise	= c: repl2 b cs (x - 1)


--((ord getChar) - 48)

> play :: Board -> IO Board
> play b = do
>			showBoard b
>			play (move b ask)

> ask :: IO Int
> ask = do
>			putStr "Where do you want to move (0-8)? "
>			n <- getChar
>			return (ord n)

-->			(ord getChar) - 48
