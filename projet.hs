import Prelude hiding (Left, Right)

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction]
  deriving (Read, Show)

type Posx = Int
type Posy = Int
type Dirx = Int
type Diry = Int
data Crayon = DonneCrayon {position::[(Posx,Posy)], direction::[(Dirx,Diry)]} --Est ce que direction n'est pas juste l'indication de l'orientation ?

data Programme = DonneProgramme [Instruction]
  deriving (Read, Show)

logoskell2svg :: Programme -> Crayon -> [String] -> (Crayon, [String]) --[String] correspond à une ligne de SVG ?
--ATTENTION : La fonction prend en entrée Programme mais aussi Crayon
--idée : Prendre programme, faire une ligne à partir de la première instruction, puis enlever la première instruction de Programme et récursion avec cette nouvelle liste et un nouveau crayon
logoskell2svg [] c = (c, acc) --acc = accumulateur qui stocke les lignes de SVG
logoskell2svg (i:progs) c
  | Forward a DonneCrayon [(px,py)] [(dx,dy)] = logoskell2svg progs DonneCrayon [(px,py)][(dx,dy)]:["<line x1=",px," y1=",py," x2=",px+a*dx," y2=",py+a*dy]:acc
  | Left a DonneCrayon [(px,py)] [(dx,dy)] = logoskell2svg progs DonneCrayon [(x,y)][(dx-1 mod 1,dy+1 mod 1)] --ne marche pas encore car je ne peux passer négatif si je tourne que d'un sens
  | Right a DonneCrayon [(px,py)] [(dx,dy)] = logoskell2svg progs DonneCrayon [(x,y)][(dx+1 mod 1,dy-1 mod 1)]



main = do
  let x = 100.000
  let y = 100.000
  let cray = DonneCrayon [(x,y)] [(0,0)]
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  let progs = DonneProgramme lesInstructions
  logoskell2svg progs cray
