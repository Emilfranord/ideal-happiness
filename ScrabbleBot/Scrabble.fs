namespace PaperScissors

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        totalPlayers  : uint32
        tileConverter : Map<uint32, tile>
        hand          : MultiSet.MultiSet<uint32>
        currentTurn   : int
        placedTiles   : Map<coord, char> 
    }

    let mkState b d pn tot til h c t = 
        {board = b; dict = d;  playerNumber = pn; totalPlayers = tot; tileConverter = til; hand = h; currentTurn = c; placedTiles = t}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let currentTurn st   = st.currentTurn
    let placedTiles st   = st.placedTiles
    let tileConverter st = st.tileConverter

    let totalPlayers st = st.totalPlayers
    let updateState st h c t = 
        {board = board st; dict = dict st; playerNumber = playerNumber st; totalPlayers = totalPlayers st; tileConverter = tileConverter st ; hand = h; currentTurn = c; placedTiles = t}


module internal Action = 
    let stepChar (tile:(uint32 * (char * int))) dict =
        let ch = tile  |> snd |> fst
        let res = Dictionary.step ch dict
        match res with
        | None -> None
        | Some (true, dic) -> Some(true, dic, tile)
        | Some (false, dic) ->  Some(false, dic, tile)
        
    let rec listWords st (prefixWord: list<(uint32 * (char * int))>) prefixDict (hand: MultiSet.MultiSet<uint32>) =
        let hand' = MultiSet.toList hand
                    |> List.map (fun identifier -> ( identifier, Map.find identifier (State.tileConverter st)))  
                    |> List.map (fun (identifier, tile) -> (Set.map (fun (ch, po) -> (identifier, (ch, po))) tile) |> Set.toList)
                    |> List.collect id

        let paths = (List.map (fun tile -> stepChar tile prefixDict) hand') |> List.choose id
        
        let finishedPaths = List.filter (fun (wordDone,_ ,_) -> wordDone = true) paths 
                            |> List.map (fun (_,_,(tile)) -> prefixWord @ (List.singleton tile))

        let recurisvePaths = List.map ( fun (_, newDict, tile) -> listWords st (prefixWord @ (List.singleton tile)) newDict (MultiSet.removeSingle (fst tile) hand)) paths

        finishedPaths @ (List.collect id recurisvePaths)

    let listWordsGivenPrefix st (prefixWord: list<char>) (hand: MultiSet.MultiSet<uint32>) = 
        let rec dictFromPrefix prefix dict = 
            match prefix with
            | head::tail -> match Dictionary.step head dict with
                            | None -> dict
                            | Some (_,b) ->  dictFromPrefix tail b 
            | [] -> dict
        
        let prefixToTiles prefix =
            List.map (fun ch -> (0u, (ch, 0))) prefix
        
        listWords st (prefixWord |> prefixToTiles) (dictFromPrefix (prefixWord) (State.dict st)) hand

    let selector (options: list<list<uint32 * (char * int)>>) =
        (List.map (fun x -> List.fold (fun acc (_, (_, point)) -> acc+point) 0 x ) options, options) 
        ||> List.map2 (fun points lst -> (points, lst))
        |> List.sortByDescending (fun (points, _) -> points)
        |> List.head

    type Direction = Right | Down

    let increaseCoord amount coord dir  = 
        match dir with
        | Right -> ((coord |> fst)+amount, coord |> snd)
        | Down ->  (coord |> fst, (coord |> snd)+amount)

    let addCoords origin tiles dir = 
        List.mapi (fun iteratror item  -> (increaseCoord iteratror origin dir, item)) tiles

    let startHooks st = 
        let coordStartsWord st (coord: (int * int)) =
            let tiles = State.placedTiles st
            // 1 2 3 
            // 4 5 6 
            // 7 8 9 
            
            let two     = Map.tryFind ((coord |> fst),      ((coord |> snd)-1)) tiles
            let three   = Map.tryFind ((coord |> fst)+1,    (coord |> snd)-1) tiles
            let four    = Map.tryFind ((coord |> fst)-1,    (coord |> snd)) tiles
            
            let six     = Map.tryFind ((coord |> fst)+1,    (coord |> snd)) tiles
            let seven   = Map.tryFind ((coord |> fst)-1,    (coord |> snd)+1) tiles
            let eight   = Map.tryFind ((coord |> fst),      (coord |> snd)+1) tiles
            let nine    = Map.tryFind ((coord |> fst)+1,    (coord |> snd)+1) tiles

            let rightAvailable = [four;nine;six;three] |> List.forall Option.isNone
            let downAvailable = [two;seven;eight;nine] |> List.forall Option.isNone
            
            let dirRight = 
                match rightAvailable with
                | true -> [Right]
                | false -> []

            let dirDown =
                match downAvailable with
                | true -> [Down]
                | false -> []

            dirRight @ dirDown
            
            

        State.placedTiles st
            |> Map.map (fun key _ -> coordStartsWord st key) 
            |> Map.filter (fun _ value -> not (List.isEmpty value) )

    let findPrefix st coord dir = 
        let rec aux st coord dir prefix = 
            let boardTiles = State.placedTiles st

            let current = Map.tryFind coord boardTiles
            let next = Map.tryFind (increaseCoord 1 coord dir) boardTiles

            let current' = match current with
                            | None -> failwith "findPrefix given a invalid coord for current"
                            | Some ch -> ch

            let prefix' = match prefix with
                            | [] -> [current']
                            | _ -> prefix

            match next with
            | None -> prefix'
            | Some ch -> aux st (increaseCoord 1 coord dir) dir (prefix' @ (List.singleton ch))
        
        (aux st coord dir List.empty, dir, coord)


    let emptyPlaces st coord dir start ending = 
        let search = (seq {start+1 .. ending+1}) |> List.ofSeq

        let w = match dir with
                | Down ->   let directFollows = List.map (fun x -> increaseCoord x coord dir ) search
                            let right = List.map (fun x -> increaseCoord x (increaseCoord 1 coord Right) dir ) search
                            let left  = List.map (fun x -> increaseCoord  x (increaseCoord -1 coord Right) dir ) search
                            directFollows @ right @ left
                        
                | Right ->  let directFollows = List.map (fun x -> increaseCoord x coord dir ) search
                            let up = List.map (fun x -> increaseCoord  x (increaseCoord 1 coord Down) dir ) search
                            let down  = List.map (fun x -> increaseCoord x (increaseCoord -1 coord Down) dir ) search
                            directFollows @ up @ down
        List.map (fun x -> Map.tryFind x (State.placedTiles st )) w  
        |> List.forall Option.isNone
        
    let constructMove st hand = 
        let cartesian xs ys = 
            xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

        let moves = (startHooks st)
                    |> Map.toList
                    |> List.map (fun (coord, dir) -> cartesian (List.singleton coord) dir) 
                    |> List.collect id
                    |> List.map (fun (coord, dir) -> findPrefix st coord dir)
                    |> List.map (fun (chars, dir, coord) -> (chars, dir, coord, chars |> List.length))
                    |> List.map (fun (prefix, dir, coord, length) -> (listWordsGivenPrefix st prefix hand , dir, coord, length))
                    |> List.map (fun (words, dir, coord, prefixLength) ->  (List.filter (fun x -> emptyPlaces st coord dir prefixLength (x |> List.length)) words, dir, coord, prefixLength))
                    |> List.filter (fun (words, _, _, _) -> not (List.isEmpty words))
                    |> List.map (fun (x, dir, coord, length) -> (x |> selector , dir, coord, length))
        
        match moves with
        | [] -> SMPass
        | a ->  a
                |> List.maxBy (fun ((points, _), _, _ ,_ ) ->  points )
                |> fun ((_, tiles), dir, startCoord, prefixLen ) ->  (addCoords startCoord tiles dir)[prefixLen ..]
                |> SMPlay

    let firstMove st hand origin = 
        let possibleWords = listWords st List.empty (State.dict st) hand    
        
        
        match possibleWords with
        | [] -> SMPass
        | a ->  a
                |> selector 
                |> fun (_, tiles) -> tiles
                |> fun tiles -> addCoords origin tiles Right
                |> SMPlay

    let action (st : State.state) = 
        match Map.tryFind (0,0) (State.placedTiles st) with
        | None -> firstMove st (State.hand st) (0,0) 
        | Some _ -> constructMove st (State.hand st)


module Scrabble =
    open System.Threading

    let updateTiles ms st = 
        let tilesToUpdate = List.map (fun (coord, (_, (char, _))) -> (coord, char)) ms
        let tiles' = List.fold (fun state (coord, char) -> Map.add coord char state) (State.placedTiles st) tilesToUpdate
        tiles'

    let updateHand removeTiles addTiles st = 
        let tilesToRemove = List.map (fun (_, (x, (_, _))) -> x) removeTiles
        let hand' = List.fold (fun state element -> MultiSet.removeSingle element state) (State.hand st) tilesToRemove
        let hand'' = List.fold (fun state (id, amount) -> MultiSet.add id amount state) hand' addTiles
        hand''

    let increasePlayerTurn st =
        (State.currentTurn st + 1) % (State.totalPlayers st |> int)
    
    let isOurTurn st =
        State.currentTurn st = (State.playerNumber st |> int) % (State.totalPlayers st |> int)
    

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            
            let w = match isOurTurn st with 
                    | true -> send cstream (Action.action st)
                    | false -> ()

            w |> ignore 
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                let tiles = updateTiles ms st
                let hand = updateHand ms newPieces st

                let st' = State.updateState st hand (increasePlayerTurn st) tiles
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                let tiles = updateTiles ms st
                let st' =  State.updateState st (State.hand st) (increasePlayerTurn st) tiles
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' =  State.updateState st (State.hand st) (increasePlayerTurn st) (State.placedTiles st)
                aux st'
            | RCM (CMPassed (pid)) -> 
                let st' =  State.updateState st (State.hand st) (increasePlayerTurn st) (State.placedTiles st)
                aux st'
            | RCM (CMForfeit (pid)) ->
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.totalPlayers st) (State.tileConverter st) (State.hand st) (State.currentTurn st) (State.placedTiles st)
                let st'' = State.updateState st' (State.hand st') (increasePlayerTurn st') (State.placedTiles st')
                aux st''
            | RCM (CMChange (playerId, numberOfTiles)) ->
                let st' = State.updateState st (State.hand st) (increasePlayerTurn st) (State.placedTiles st)
                aux st'
            | RCM (CMChangeSuccess (lst)) ->
                let hand = updateHand List.empty lst st
                let st' = State.updateState st (hand) (increasePlayerTurn st) (State.placedTiles st)
                aux st'
            | RCM (CMTimeout (pid)) -> 
                let st' = State.updateState st (State.hand st) (increasePlayerTurn st) (State.placedTiles st)
                aux st'
            | RCM (CMGameOver _) -> ()
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // true if using a gaddag for dictionary
        //let dict = dictf false // false if using a trie for dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers tiles handSet (playerTurn |> int) Map.empty<coord, char>)
        