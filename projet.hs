import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

type Posx = Int
type Posy = Int
type Dirx = Int
type Diry = Int
data Crayon = DonneCrayon {position::[(Posx,Posy)], direction::[(Dirx,Diry)]}

data Programme = DonneProgramme (IO [Instruction])
  deriving (Read, Show)





main = do
  let cray = DonneCrayon [(100,100)] [(0,0)]
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  let progs = DonneProgramme lesInstructions
  return progs
