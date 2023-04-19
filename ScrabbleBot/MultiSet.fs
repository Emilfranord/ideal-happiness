// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = M of (Map<'a, uint32>) 
    let empty = M (Map.empty<'a, uint32>)
    let isEmpty (M s) = Map.isEmpty s 
    let size (M s) = Map.fold (fun state _ value -> state + value) 0u s 
    let contains element (M set) = Map.containsKey element set
    let numItems element (M set) = Map.tryFind element set |> Option.defaultValue 0u
    let add element amount (M set) = 
        let newAmount = amount + numItems element (M set)
        Map.add element newAmount set |> M
    let addSingle element (M s) = add element 1u (M s)
    let remove element amount (M set) = 
        let inSet = numItems element (M set) 
        match inSet with
        | n when n <= amount -> Map.remove element set |> M
        | _ -> Map.add element (inSet - amount) set |> M

    let removeSingle element (M set) = remove element 1u (M set)
    let fold folder acc (M set) = Map.fold folder acc set
    let foldBack folder (M set) acc = Map.foldBack folder set acc
    let rec ofList = 
        function
        | [] -> empty
        | head::tail -> addSingle head (ofList tail)
    
    let rec toList (M set) =
        match Map.toList set with
        | [] -> List.empty
        | head::tail -> List.init (head |> snd |> int) (fun _ -> (head |> fst)) @ (tail |> Map.ofList |> M |> toList)

    let map f set = toList set |> List.map f |> ofList 

    let union (M a) (M b) = Map.toList a @ Map.toList b |> List.sort |> Map.ofList |> M

    let sum a b = 
       ( a |> toList) @ ( b |> toList) |> ofList

    let subtract a b = List.except ( b |> toList) ( a |> toList)  |> ofList

    let toMap (M a) = a 

    let intersection (M a) (M b) = 
        let uni = union (M a) (M b) |> toMap 
        Map.filter (fun x _ -> (Map.containsKey x uni)) uni |> M