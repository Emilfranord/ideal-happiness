// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = M of (Map<'a, uint32>) 

    let empty : MultiSet<'a> = M (Map.empty<'a, uint32>)

    let numItems element (M set) = Map.tryFind element set |> Option.defaultValue 0u

    let add element amount (M set) = 
        let newAmount = amount + numItems element (M set)
        Map.add element newAmount set |> M

    let fold folder acc (M set) = 
        Map.fold folder acc set
