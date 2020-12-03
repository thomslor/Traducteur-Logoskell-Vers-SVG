import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

type Posx = Int
type Posy = Int
type Dirx = Int
type Diry = Int
data Crayon = DonneCrayon {position::[(Posx,Posy)], direction::[(Dirx,Diry)]}

data Programme = DonneProgramme [Instruction]
  deriving (Read, Show)

logoskell2svg :: Programme -> Crayon -> [String] -> (Crayon, [String]) --[String] correspond à une ligne de SVG
--idée : Prendre programme, faire une ligne à partir de la première instruction, puis enlever la première instruction de Programme et récursion avec cette nouvelle liste
logoskell2svg progs



main = do
  --let cray = DonneCrayon [(100,100)] [(0,0)]
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  let progs = DonneProgramme lesInstructions
  logoskell2svg progs
