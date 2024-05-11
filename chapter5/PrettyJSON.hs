
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape
                      | otherwise    -> char class  where
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

string :: String -> Doc
string = enclose  '"' '"' . hcat . map . oneChar

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch