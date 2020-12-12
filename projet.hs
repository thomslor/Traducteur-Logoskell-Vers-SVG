import Prelude hiding (Left, Right)

--Creation de differents types pour les instructions, le programmeet le crayon
data Instruction = Forward Float | Left Float | Right Float | Repeat Int [Instruction]
  deriving (Read, Show)

data Crayon = DonneCrayon {positionX::Float, positionY::Float, angle::Float}
  deriving (Read, Show)

type Programme = [Instruction]

-- Fonction qui va traiter les differentes instructions en entree pour les convertir au format svg sous forme d'une liste de String
logoskell2svg :: Programme -> Crayon -> [String] -> (Crayon, [String])
--ATTENTION : La fonction prend en entrée Programme mais aussi Crayon
-- c == Crayon
logoskell2svg [] c res = (c, res)
logoskell2svg (x:xs) c res = case x of
  Forward i -> logoskell2svg xs (DonneCrayon (i * cos agl) (i * sin agl) (angle c)) (res ++ ["<line x1=\"" ++ (show (positionX c)) ++ "\" y1=\"" ++ (show (positionY c)) ++ "\" x2=\"" ++ (show (i * cos agl)) ++ "\" y2=\"" ++ (show (i * sin agl)) ++ "\" stroke=\"red\" />\n"])
  Left i -> logoskell2svg xs (DonneCrayon (positionX c) (positionY c) (((angle c)+i))) res
  Right i -> logoskell2svg xs (DonneCrayon (positionX c) (positionY c) (((angle c)-i))) res
  Repeat i j -> logoskell2svg ((take ((length j) * i)(cycle j)) ++ xs) c res
  where
    agl = angle c/(pi/2)

--Fonction qui va mettre l'ensemble des lignes de la liste de String sur une seule ligne (un seul String)
oneLine :: (Crayon, [String]) -> String
oneLine (_, []) = ""
oneLine (c, x:xs) =
  x ++ oneLine(c, xs)

-- Fonction qui permet de respecter la syntaxe svg (entete et pied de page)
outputStrLn :: String -> String
outputStrLn x = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"500\" height=\"500\">\n<title>Exemple</title>\n"++x++"</svg>"

main :: IO()
main = do
  --initialisation du crayon de base
  let x = 100.0
  let y = 100.0
  let cray = DonneCrayon x y 0.0
  --Entree des instructions par l'utilisateur
  putStrLn "Entrez Instructions : "
  line <- getLine
  let lesInstructions = (read line :: [Instruction])
  let prog = lesInstructions
  --composition de toutes nos fonctions 
  putStrLn (outputStrLn(oneLine(logoskell2svg prog cray [])))
  
