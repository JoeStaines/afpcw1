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
