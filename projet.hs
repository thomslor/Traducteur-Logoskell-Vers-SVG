import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

lecture :: IO [Instruction]
lecture = do
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  return (lesInstructions)

type posx = Int
type posy = Int
type dirx = Int
type diry = Int
data Crayon :: DonneCrayon {position :: [(posx,posy)], direction :: [(dirx,diry)]}
