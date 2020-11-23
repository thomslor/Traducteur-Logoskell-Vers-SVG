import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

lecture :: IO [Instruction]
lecture = do
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstruction = (read line :: [Instruction])
  return (lesInstruction)
