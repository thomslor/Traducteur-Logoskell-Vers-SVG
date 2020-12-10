import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

data Crayon = DonneCrayon {positionX::Int, positionY::Int, angle::Int}
  deriving (Read, Show)

data Programme = DonneProgramme [Instruction]
  deriving (Read, Show)

logoskell2svg :: Programme -> Crayon -> [String]
--ATTENTION : La fonction prend en entrée Programme mais aussi Crayon
--idée : Prendre programme, faire une ligne à partir de la première instruction, puis enlever la première instruction de Programme et récursion avec cette nouvelle liste et un nouveau crayon
logoskell2svg [] c = []
logoskell2svg a c
  | c == 0 = ["<line x1="positionX c" y1="positionY c" x2="positionX c+a" y2="positionY]
  | c == pi/2 || -3pi/2 = ["<line x1="positionX c" y1="positionY c" x2="positionX c" y2="positionY c+a]
  | c == pi || -pi = ["<line x1="positionX c" y1="positionY c" x2="positionX c-a" y2="positionY c]
  | c == 3pi/2 || -pi/2 = ["<line x1="positionX c" y1="positionY c" x2="positionX c" y2="positionY c-a]
logoskell2svg (x:xs) c
  | Forward a DonneCrayon positionX positionY angle = logoskell2svg (xs) DonneCrayon positionX positionY angle
  | Left a DonneCrayon positionX positionY angle = logoskell2svg (xs) DonneCrayon positionX positionY angle + pi/2 mod 2*pi
  | Right a DonneCrayon positionX positionY angle = logoskell2svg (xs) DonneCrayon positionX positionY angle - pi/2 mod 2*pi



main = do
  let x = 100
  let y = 100
  let cray = DonneCrayon x y 0
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  let prog = DonneProgramme lesInstructions
  logoskell2svg prog cray
