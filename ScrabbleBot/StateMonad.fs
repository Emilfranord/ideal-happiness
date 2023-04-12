// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))

    let wordLength : SM<int> = S (fun s -> Success (List.length s.word, s))

    let characterValue (pos : int) : SM<char> =
        let finder s = 
            match List.tryItem pos s.word with
            | Some a -> Success (fst a, s)
            | None  -> Failure (IndexOutOfBounds pos)
        S (finder)

    let pointValue (pos : int) : SM<int> =
        let finder s = 
            match List.tryItem pos s.word with
            | Some a -> Success (snd a, s)
            | None  -> Failure (IndexOutOfBounds pos)
        S (finder)  

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let update (var : string) (value : int) : SM<unit> =
        let rec aux depth lst =
            match depth, lst with
            | _, []      -> None
            | d, m :: ms -> 
                match Map.tryFind var m with
                | Some _ -> Some d
                | None   -> aux (d+1) ms

        S (fun s -> 
              match aux (0) (s.vars) with
              | Some index -> Success ((), 
                                {s with vars =    (s.vars.[..index-1]) 
                                                @ (s.vars.[index] |> Map.add var value|> List.singleton) 
                                                @ (s.vars.[index+1..])})
              | None   -> Failure (VarNotFound var))
    
    let declare (var : string) : SM<unit> =
        let finder state = 
            let notReserved = Set.contains var state.reserved  |> not
            let notExists = Map.containsKey var state.vars.Head |> not

            match notReserved, notExists with
            | true, true -> Success ((), {state with vars = (state.vars.Head |> Map.add var 0) :: state.vars.Tail})
            | true, false -> Failure (VarExists var)
            | false, _ -> Failure (ReservedName var)
        
        S (finder)  