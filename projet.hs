import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

data Crayon = DonneCrayon {positionX::Int, positionY::Int, angle::Int}
  deriving (Read, Show)

data Programme = DonneProgramme {instructions::[Instruction]}
  deriving (Read, Show)


logoskell2svg :: Programme -> Crayon -> [String]
--ATTENTION : La fonction prend en entrée Programme mais aussi Crayon
--idée : Prendre programme, faire une ligne à partir de la première instruction, puis enlever la première instruction de Programme et récursion avec cette nouvelle liste et un nouveau crayon
-- c = Crayon
logoskell2svg [] c = []
logoskell2svg (Forward a) c --PROBLEME : On arrive pas à mettre une instruction en paramètre
  | angle c == 0 = ("<line x1=" ++ (show (positionX c)) ++ " y1=" ++ (show (positionY c)) ++ " x2=" ++ (show ((positionX c) + a)) ++ " y2=" ++ (show (positionY c)))
  | angle c == 90 || angle c == -270 = ("<line x1=" ++ (show (positionX c)) ++ " y1=" ++ (show (positionY c)) ++ " x2=" ++ (show (positionX c)) ++ " y2=" ++ (show (positionY c+a)))
  | angle c == 180 || angle c == -180 = ("<line x1=" ++ (show (positionX c)) ++ " y1=" ++ (show (positionY c)) ++ " x2=" ++ (show (positionX c-a)) ++ " y2=" ++ (show (positionY c)))
  | angle c == 270 || angle c == -90 = ("<line x1=" ++ (show (positionX c)) ++ " y1=" ++ (show (positionY c)) ++ " x2=" ++ (show (positionX c)) ++ " y2=" ++ (show (positionY c-a)))

logoskell2svg (x:xs) c
  | x == (Forward a) && angle c == 0 = logoskell2svg (xs) DonneCrayon (positionX+a) positionY angle
  | x == (Forward a) && angle c == 90 || angle c == -270 = logoskell2svg (xs) DonneCrayon positionX (positionY+a) angle
  | x == (Forward a) && angle c == 180 || angle c == -180 = logoskell2svg (xs) DonneCrayon (positionX-a) positionY angle
  | x == (Forward a) && angle c == 270 || angle c == -90 = logoskell2svg (xs) DonneCrayon positionX (positionY-a) angle
  | x == (Left a) = logoskell2svg (xs) DonneCrayon positionX positionY ((angle + a) `mod` 360)
  | x == (Right a) = logoskell2svg (xs) DonneCrayon positionX positionY ((angle - a) `mod` 360)


repeat :: Programme -> Programme
repeat [] = []
repeat (x:xs)
  | x == Repeat a [b] = b:xs
  | otherwise = repeat xs

main = do
  let x = 100
  let y = 100
  let cray = DonneCrayon x y 0
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: Instruction)
  return lesInstructions
  --logoskell2svg (lesInstructions) (cray)
  --let prog = DonneProgramme lesInstructions
  --return prog
