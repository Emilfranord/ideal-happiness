// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar" <?> "intToChar"
    let pPointValue = pstring "pointValue" <?> "pointValue"

    let pCharToInt  = pstring "charToInt" <?> "charToInt"
    let pToUpper    = pstring "toUpper" <?> "toUpper"
    let pToLower    = pstring "toLower" <?> "toLower"
    let pCharValue  = pstring "charValue" <?> "charValue"

    let pTrue       = pstring "true" <?> "true"
    let pFalse      = pstring "false" <?> "false"
    let pIsDigit    = pstring "isDigit" <?> "isDigit"
    let pIsLetter   = pstring "isLetter" <?> "isLetter"
    let pIsVowel   = pstring "isVowel" <?> "isVowel"
    let pIsConsonant = pstring "isConsonant" <?> "isConsonant"

    let pif       = pstring "if" <?> "if"
    let pthen     = pstring "then" <?> "then"
    let pelse     = pstring "else" <?> "else"
    let pwhile    = pstring "while" <?> "while"
    let pdo       = pstring "do" <?> "do"
    let pdeclare  = pstring "declare" <?> "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b = (a .>> spaces) .>>. b
    let (.>*>) a b   = (a .>> spaces) .>> b
    let (>*>.) a b   = (a .>> spaces) >>. b

    let surroundedBy p s e = pchar s >*>. p .>*> pchar e
    
    let parenthesise p = surroundedBy p '(' ')' <?> "parenthesise"
    let brackets p     = surroundedBy p '[' ']' <?> "brackets"
    let braces p       = surroundedBy p '{' '}' <?> "braces"
    
    let quotes p       = pchar '\"' >>. p .>> pchar '\"' <?> "quotes"
    let singleQuotes p = pchar '\'' >>. p .>> pchar '\'' <?> "singleQuotes"

    let innerCombine (a: Parser<char * list<char>>)  =  a |>> (fun (x, xs) -> x :: xs)
    let stringconverter (lst: list<char>) = System.String.Concat(Array.ofList(lst))
    let pid = (pletter <|> pchar '_') .>>. (many (palphanumeric <|> pchar '_')) |> innerCombine |>> stringconverter <?> "Identifier"

    
    let unop op a= op >*>. a
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let BoolParseLoose, blref = createParserForwardedToRef<bExp>()
    let BoolParseMed, bmref = createParserForwardedToRef<bExp>()
    let BoolParseHard, bhref = createParserForwardedToRef<bExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse;]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pstring "%") AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"
    let pointValueParse = unop pPointValue AtomParse |>> PV <?> "PointValue"
    let variableParse = pid |>> V <?> "Variable"
    let CharToInt = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; NParse; pointValueParse; CharToInt; ParParse; variableParse]

    let AexpParse = TermParse

    let parenthesiseChar = parenthesise CharParse
    let LiteralChar = singleQuotes (palphanumeric <|> whitespaceChar) |>> C <?> "LiteralChar"
    let CharValue = unop pCharValue (parenthesise AtomParse) |>> CV <?> "CharValue"
    let IntToChar = unop pIntToChar (parenthesise AtomParse) |>> IntToChar <?> "IntToChar"
    let ToUpper = unop pToUpper parenthesiseChar |>> ToUpper <?> "ToUpper"
    let ToLower = unop pToLower parenthesiseChar |>> ToLower <?> "ToLower"

    do cref := choice [LiteralChar; CharValue; IntToChar; ToUpper; ToLower; parenthesiseChar]

    let CexpParse = CharParse


    let AndParse = binop (pstring "/\\") BoolParseMed BoolParseLoose |>> Conj <?> "Conj"
    let OrParse = binop (pstring "\\/") BoolParseMed BoolParseLoose |>> (fun (a, b) -> (Not a, Not b) |> Conj |> Not) <?> "Or"

    do blref := choice [AndParse; OrParse; BoolParseMed]

    let EqualParse = binop (pchar '=') AtomParse AtomParse |>> AEq <?> "Equal"
    let NotEqual = binop (pstring "<>") AtomParse AtomParse |>> (fun (a, b) -> AEq (a, b) |> Not ) <?> "Equal"
    let LessThanParse = binop (pchar '<') AtomParse AtomParse |>> ALt <?> "LessThan"

    //let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let LessThanOrEqual = binop (pstring "<=") AtomParse AtomParse |>> (fun (a, b) -> (Not (ALt (a, b)), (AEq (a, b) |> Not) |> Not |> Not) |> Conj |> Not) <?> "LessThanOrEqual"

    // ~~(a .=. b) .&&. (a .>=. b)
    let MoreThanParse = binop (pchar '>') AtomParse AtomParse |>> (fun (a, b) -> (Not (AEq (a, b)), Not (ALt (a, b))) |> Conj) <?> "MoreThan"
    let MoreThanOrEqual = binop (pstring ">=" ) AtomParse AtomParse |>> (fun (a, b) -> ALt (a, b) |> Not) <?> "LessThan"


    do bmref := choice [EqualParse; NotEqual; LessThanParse; LessThanParse; LessThanOrEqual; MoreThanParse; MoreThanOrEqual; BoolParseHard]

    let BParParse = parenthesise BoolParseLoose
    let TrueParse = pTrue |>> (fun _ -> TT) <?> "True" 
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "False" 
    let NotParse = unop (pchar '~') BoolParseLoose |>> Not <?> "Not" 
    let IsConsonantParse = unop pIsConsonant (parenthesise CharParse) |>> IsConsonant <?> "IsConsonant"
    let IsVowelParse = unop pIsVowel (parenthesise CharParse) |>> IsVowel <?> "IsVowel"

    do bhref := choice [TrueParse; FalseParse; NotParse; IsConsonantParse; IsVowelParse; BParParse]

    let BexpParse = BoolParseLoose

    let stmntParse = pstring "not implemented"

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
