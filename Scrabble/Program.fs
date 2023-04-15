// Learn more about F# at http://fsharp.org

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main _ =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()
    
//  let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

//    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

//    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words     = readLines "./Scrabble/Dictionaries/English.txt"

    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI =
        // set dictAPI to None, to use the build-in dict.
        // otherwise give a touple with the four methods relevant to a dict.
        // last element None if you have not implemented a GADDAG
        // Some (Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse) 
        Some (Dictionaries.Trie.empty, Dictionaries.Trie.insert, Dictionaries.Trie.step, None)


    let (dictionary, time) =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)

    let players     = [("PaperScissors", dictionary, PaperScissors.Scrabble.startGame); ("OxyphenButazone", dictionary, Oxyphenbutazone.Scrabble.startGame)]

    //let players     = [("PaperScissors",dictionary, PaperScissors.Scrabble.startGame)]

    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2

    do ScrabbleServer.Comm.startGame 
        board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore

    0
