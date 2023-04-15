namespace Dictionaries

module internal NaiveDict = 
    type Dict = D of Set<string>

    let empty () = Set.empty<string> |> D

    let insert item (D set) = Set.add item set |> D

    let lookup item (D set) = Set.contains item set