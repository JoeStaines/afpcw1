G52AFP Coursework 1 - Noughts and crosses
   
James Key
psyjpk@nottingham.ac.uk
Joe Staines
psyjs4@nottingham.ac.uk

----------------------------------------------------------------------

For flexibility, the size of the board is defined as a constant:

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

> turn :: Board -> Player
> turn xss = turnAux (playerAmount xss Cross) (playerAmount xss Nought)

> playerAmount :: Board -> Player -> Int
> playerAmount xss pl = length( filter (==pl) (concat xss))

> turnAux :: Int -> Int -> Player
> turnAux x y 
>			| x == y 		= Nought
>			| otherwise 	= Cross 

> winner :: Board -> Maybe Player
> winner b 	| playerAmount Blank == 0 = Nothing
>			| otherwise = rcheck b

> rCheck [] = false
> rCheck (b:bs) x = any longAssCheck b : rcheck bs

> longAssCheck b
	|((b !! 0) !! 0 == (b !! 0) !! 1 && (b !! 0) !! 1 == (b !! 0) !! 2) = True
	|((b !! 1) !! 0 == (b !! 1) !! 1 && (b !! 1) !! 1 == (b !! 1) !! 2) = True
	|((b !! 2) !! 0 == (b !! 2) !! 1 && (b !! 2) !! 1 == (b !! 2) !! 2) = True
> wincheck :: Board -> Bool
> wincheck b = foldr (&&) True b
 

--transpose gets cols, existing lists get rows