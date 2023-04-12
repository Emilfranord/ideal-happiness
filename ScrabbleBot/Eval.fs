// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add (a: SM<int>) (b: SM<int>)  =
        a >>= fun x -> 
        b >>= fun y -> 
        ret (x + y)

    let div a b =
        a >>= fun x -> 
        b >>= fun y ->     
        if y <> 0 then ret (x / y) else fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b : SM<'d>=
        a >>= fun x ->
        b >>= fun y ->
        ret (f x y)

    let zeroSafeBinop f a b  = 
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (f x y) else fail DivisionByZero


    let rec arithEval expression: SM<int> = 
        match expression with
        | N n -> ret n
        | V str -> lookup str 
        | WL -> wordLength
        | PV pos -> arithEval pos >>= (fun index -> pointValue index)
        | Add (a,b) -> binop ( + ) (arithEval a) (arithEval b)
        | Sub (a,b) -> binop ( - ) (arithEval a) (arithEval b)
        | Mul (a,b) -> binop ( * ) (arithEval a) (arithEval b)
        | Div (a,b) -> zeroSafeBinop ( / ) (arithEval a) (arithEval b)
        | Mod (a,b) -> zeroSafeBinop ( % ) (arithEval a) (arithEval b)
        | CharToInt expres -> charEval expres >>= fun symbol -> ret (int symbol)
    and charEval expression : SM<char> = 
        match expression with
        | C char -> ret char
        | CV posExp -> arithEval posExp >>= (fun index -> characterValue index)
        | ToUpper exp -> charEval exp >>= fun symbol -> ret ( System.Char.ToUpper symbol)
        | ToLower exp -> charEval exp >>= fun symbol -> ret ( System.Char.ToLower symbol)
        | IntToChar exp -> arithEval exp >>= (fun number -> ret (number.ToString().[0]))

    let isVowel c =
        match System.Char.ToUpper c with
        | 'A' |'E' |'I' | 'O' | 'U' -> true
        | _ -> false

    let rec boolEval expression : SM<bool> =
        match expression with
        | TT -> ret true
        | FF -> ret false

        | AEq (a, b) -> binop ( = ) (arithEval a) (arithEval b)
        | ALt (a, b) -> binop ( < ) (arithEval a) (arithEval b)

        | Not a -> boolEval a >>= fun bool -> ret (not bool)
        | Conj (a, b) -> binop ( && ) (boolEval a) (boolEval b)

        | IsVowel exp -> charEval exp >>= fun symbol -> ret ( isVowel symbol)
        | IsConsonant exp -> charEval exp >>= fun symbol -> ret ( symbol |> isVowel |> not )


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare str           -> declare str
        | Ass (str, aExp)       -> (arithEval aExp) >>= fun value -> update str value 
        | Skip                  -> ret ()
        | Seq (stmA, stmB)      -> stmntEval stmA >>= fun _ -> stmntEval stmB >>= fun _ -> ret ()
        | ITE (boolExp, stmA, stmB)-> boolEval boolExp >>= fun bo -> 
                                    (if bo 
                                    then push >>>= stmntEval stmA >>>= pop 
                                    else push >>>= stmntEval stmB >>>= pop)
        | While (boolExp, stm)     -> boolEval boolExp >>= fun bo -> 
                                    (if bo 
                                    then push >>>= stmntEval stm >>= fun _ -> stmntEval (While (boolExp, stm)) >>>= pop 
                                    else ret ())

(* Part 4 *) 
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"