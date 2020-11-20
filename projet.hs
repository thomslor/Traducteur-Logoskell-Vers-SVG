import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)
