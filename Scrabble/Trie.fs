namespace Dictionaries

module internal Trie = 
    type Dict = 
        | Leaf of bool
        | Node of bool*Map<char, Dict>

    let empty () = Leaf false

    let rec insert (s:string) (d:Dict) =
        match d with
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (b,m) when s.Length = 0 -> Node (true,m)

        | Leaf b -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) Map.empty)
        | Node (b,m) -> 
            match Map.tryFind s.[0] m with
                | Some d -> Node (b, Map.add (s.[0]) (insert s.[1..] d) m) 
                | None -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) m) 


    let rec lookup (s:string) (d:Dict) =
        match d with
            | Leaf b when s.Length = 0 -> b
            | Leaf _ -> false
            | Node (b,_) when s.Length = 0 -> b
            | Node (_ ,m) ->
                match m.TryFind(s.[0]) with
                    | Some d -> lookup s.[1..] d
                    | None -> false

    let step (c:char) (d:Dict) =
        match d with
        | Node (b, m) ->
            match Map.tryFind c m with
                | Some d -> 
                    match d with
                        | Leaf b -> Some (b, d)
                        | Node (b,m) -> Some (b, d)
                | None -> None
        | Leaf _ -> None
