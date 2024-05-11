import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

string :: String -> Doc
string s = undefined

text :: String -> Doc
text s = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined