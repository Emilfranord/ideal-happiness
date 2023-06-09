namespace Dictionaries
(*Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse*)

(*https://ericsink.com/downloads/faster-scrabble-gordon.pdf*)
(*Assignment 4*)


module internal GADDAG =
    type Symbol =   | Blank
                    | Character of char

    type Dict = 
        | Leaf of bool
        | Node of bool*Map<Symbol, Dict>

    let empty () = Leaf false

    let rec insertSingle  (s : List<Symbol>) (d: Dict) =
        match d with
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (_,m) when s.Length = 0 -> Node (true,m)

        | Leaf b -> Node (b, Map.add (s.[0]) (insertSingle s.[1..] (empty())) Map.empty)
        | Node (b,m) -> 
            match Map.tryFind s.[0] m with
                | Some d -> Node (b, Map.add (s.[0]) (insertSingle s.[1..] d) m) 
                | None -> Node (b, Map.add (s.[0]) (insertSingle s.[1..] (empty())) m) 

    let insert (s: string) (d: Dict) = 
        let sequenceBuilder (str: string) (index: int) = 
            let prefix  = List.map Character (Seq.toList (str.[..index]))
            let postfix = List.map Character (Seq.toList (str.[(index+1)..]))
            (List.rev prefix) @ ([Blank]) @ postfix

        let word = List.map Character (Seq.toList (s))

        let list = List.mapi (fun index _ -> sequenceBuilder s index) (Seq.toList s) @ List.singleton word

        List.fold (fun state element -> insertSingle element state) d list

    let step (ch: char) (dict: Dict) = 
        let symbol = 
            match ch with
            | '0' -> Blank
            | a -> Character a

        match dict with
        | Node (_, m) ->
            match Map.tryFind symbol m with
                | Some dict' -> 
                    match dict' with
                        | Leaf b -> Some (b, dict')
                        | Node (b,_) -> Some (b, dict')
                | None -> None
        | Leaf _ -> None


    let reverse (d: Dict) = 
        step '0' d