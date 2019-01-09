module Chess
open System.Text.RegularExpressions
type Color = White | Black
type Position = int * int

/// An abstract chess piece
[<AbstractClass>]
type chessPiece ( color : Color ) =
  let mutable _position : Position option = None
  abstract member nameOfType : string // "king", "rook", ...
  member this.color = color // White, Black
  member this.position // E.g., (0,0), (3,4), etc.
    with get () = _position
    and set ( pos ) = _position <- pos
  override this.ToString () = // E.g. "K" for white king
    match color with
      White -> ( string this.nameOfType.[0]).ToUpper ()
      | Black -> ( string this.nameOfType.[0]).ToLower ()
  abstract member candidateRelativeMoves : Position list list

  ///<summary>The member availableMoves returns a chessPiece available moves
  /// and opponent pieces that can be attacked </summary>
  ///<param name="board"> A Board type is provided as parameter</param>
  ///<remarks>Will only work in this framework</remarks>
  ///<returns>A tuple containg a list of position(s) and a list of chessPiece(s)</returns>
  member this.availableMoves (board:Board) : (Position list * chessPiece list) =
    let moves = board.getVacantNNeighbours this
    let mutable notSafeMoves = []
    let mutable (p:chessPiece option) = board.Item(0,0)
    if (this.nameOfType = "king") then
      let king = this
      board.[(fst king.position.Value),(snd king.position.Value)] <- None
      for i in 0..7 do
        for j in 0..7 do
          p <- board.Item(i,j)
          if ((p.IsSome) && (this.color <> p.Value.color)) then
              notSafeMoves <- List.append notSafeMoves (fst (board.getVacantNNeighbours p.Value))
      let safeMoves = fst moves |> List.filter (fun x -> not(List.contains x notSafeMoves))
      board.[(fst king.position.Value), (snd king.position.Value)] <- Some king
      (safeMoves,(snd moves))

    else board.getVacantNNeighbours this

/// A board
and Board () =
  let _array = Collections.Array2D.create<chessPiece option> 8 8 None
  /// Wrap a position as option type
  let validPositionWrap ( pos : Position ) : Position option =
    let (rank, file) = pos // square coordinate
    if rank < 0 || rank > 7 || file < 0 || file > 7 then None
    else Some (rank, file)
    /// Convert relative coordinates to absolute and remove
    /// out - of - board coordinates .
  let relativeToAbsolute (pos:Position) (lst:Position list) : Position list =
    let addPair (a:int,b:int) (c:int,d:int) : Position = (a+c, b+d)
    // Add origin and delta positions and Choose absolute positions
    // that are on the board
    List.map (addPair pos) lst |> List.choose validPositionWrap
  /// Board is indexed using.[ ,] notation
  member this.Item
    with get (a:int, b:int) = _array.[a, b]
    and set (a:int, b:int) (p:chessPiece option) =
      if p.IsSome then p.Value.position <- Some (a, b)
      _array.[a, b] <- p
  /// Produce string of board for, e.g., the printfn function.
  override this.ToString() =
    let letters = [for i in 1..8 -> char(96+i)]
    let rec boardStr (i : int) (j : int) : string =
      match (i,j) with
        (8,0) -> ""
        | _ ->
          let stripOption (p : chessPiece option) : string =
            match p with
              None -> ""
              | Some p -> p.ToString()
          // print top to bottom row
          let pieceStr = stripOption _array.[7-i,j]
          //let pieceStr = sprintf "(%d, %d)" i j
          let lineSep = " " + String.replicate (8*4-1) "-"
          match (i,j) with
          (0,0) ->
            let str = sprintf "%s\n| %1s " lineSep pieceStr
            str + boardStr 0 1
          | (i,7) ->
            let str = sprintf "| %1s |%c\n%s\n" pieceStr letters.[(abs(i-7))] lineSep // just to make it easy to debug (abs(i-7))
            str + boardStr (i+1) 0
          | (i,j) ->
            let str = sprintf "| %1s " pieceStr
            str + boardStr i (j+1)
    boardStr 0 0
    /// Move piece by specifying source and target coordinates
  member this.move (source:Position) (target:Position) : unit =
    this.[fst target, snd target] <- this.[fst source, snd source]
    this.[fst source, snd source] <- None
  /// Find the tuple of empty squares and first neighbour if any.
  member this.getVacantNOccupied (run:Position list) : (Position list * (chessPiece option)) =
    try
      // Find index of first non-vacant square of a run
      let idx = List.findIndex (fun(i,j) -> this.[i,j].IsSome) run
      let (i,j) = run.[idx]
      let piece = this.[i,j] // The first non-vacant neighbour
      if idx = 0
      then ([],piece)
      else (run.[..(idx-1)],piece)
    with
      _ -> ( run , None ) // outside the board
  /// find the list of all empty squares and list of neighbours
  member this.getVacantNNeighbours ( piece : chessPiece ) : ( Position list * chessPiece list ) =
    match piece.position with
      None -> ([] ,[])
      | Some p ->
        let convertNWrap =
          (relativeToAbsolute p) >> this.getVacantNOccupied
        let vacantPieceLists = List.map convertNWrap piece.candidateRelativeMoves
        // Extract and merge lists of vacant squares
        let vacant = List.collect fst vacantPieceLists
        // Extract and merge lists of first obstruction pieces and filter out own pieces
        let opponent =
          vacantPieceLists
          |> List.choose snd |> List.filter (fun c -> c.color <> piece.color)
        (vacant, opponent)

/// <summary>The player class is the abstract class, a player can either
/// be a human player or the computer (not yet implemented)</summary>
/// <param name="c">c is a Color type, either Black or White</param>
/// <remarks>Don't try passing pink as an argument!</remarks>
/// <returns>A player object with the given Color</returns>
[<AbstractClass>]
type Player (c: Color) =
  member this.color = c
  abstract member nameOfType : string
  abstract member nextMove : Board -> string

/// <summary>The human class needs a human player to provide
/// input to get the game going</summary>
/// <param name="c">c is a Color, provided to instantiate the object</param>
/// <remarks>Cannot handle any thing else, it is weak!</remarks>
/// <returns>A Player.Human object</returns>
type Human (c: Color) =
  inherit Player(c)
  /// <summary>A simple override to get a string
  ///  representation of the player</summary>
  /// <returns>A string (white or black)</returns>
  override this.nameOfType = string(this.color)
  /// <summary> The member nextMove returns the next move to the Game class
  /// Passing the players move as a string </summary>
  /// <param name="b">b is a Board object</param>
  /// <remarks> Will not stop until a valid input is given
  /// or the palyer is checkmate</remarks>
  /// <returns> A string representing the move</returns>
  override this.nextMove (b: Board): string =
    let mutable str = ""
    let mutable isValid = false
    /// Checking if the user is checkmate
    while not(isValid) do
      let mutable (p: chessPiece option) = b.Item(0,0)
      for i in 0..7 do
        for j in 0..7 do
          p <- b.Item(i,j)
          if (p.IsSome) then
            if ((p.Value.nameOfType = "king") && (p.Value.color = this.color)) then
              let m = List.append (fst (p.Value.availableMoves b)) [for e in (snd (p.Value.availableMoves b)) -> e.position.Value]
              if (m.IsEmpty) then
                printfn "Checkmate %s!" this.nameOfType
                str <- "quit"
                isValid <- true

      /// If not checkmate prompt user for input
      if not(isValid) then
        printf "%s's turn " this.nameOfType
        str <- System.Console.ReadLine()
        let (r:Match) = Regex.Match(str, @"[a-h][1-8]\s[a-h][1-8]")
        /// Quitting sequence if player wants to quit
        if (str = "quit") then
          isValid <- true
        elif (r.Success) then
          let startPos = ((int(str.[0])-97), (int(str.[1])-49))
          let endPos = ((int(str.[3])-97), (int(str.[4])-49))
          p <- b.Item(fst startPos, snd startPos)
          if ((p.IsSome) && (endPos < (8,8)) && (endPos > (-1,-1))) then
            let moveList = List.append (fst (p.Value.availableMoves b)) [for e in (snd (p.Value.availableMoves b)) -> e.position.Value]
            if ((p.Value.color = this.color)) && ((List.contains endPos moveList)) then
              isValid <- true
            elif (moveList.IsEmpty) then
              str <- "quit"
              isValid <- true
    str

/// <summary>The Game class containts the logic of the game loop. Which
///  player turn it is, what move to make and refreshing the screen</summary>
/// <param name="player1">A player object (human or computer)</param>
/// <param name="player2">A player object (human or computer)</param>
/// <remarks>Computer is not implemented yet</remarks>
/// <returns>An object that can be minipulated</returns>
type Game (player1 : Player, player2: Player) =
  member this.p1 = player1
  member this.p2 = player2
  /// <summary>The member run takes care of each loop, and ends the game
  /// when checkmate or quit code is given</summary>
  /// <param name="b">b is a Board</param>
  /// <remarks>Does not need to be run through a loop, since it does so itself</remarks>
  /// <returns>An unit</returns>
  member this.run (b: Board) =
    let board = b
    let mutable cPlayer = this.p1
    let mutable codeString = ""
    let mutable turn = 1
    let makeNums() =
      for i in 1..8 do
        printf "| %d " i
        if i = 8 then printfn "|"
    makeNums()
    printfn "%A" board
    while (codeString <> "quit") do
      if (turn % 2 = 0) then
        cPlayer <- this.p2
      else cPlayer <- this.p1

      let move = cPlayer.nextMove board
      if (move = "quit" ) then
        codeString <- "quit"
        printfn "Game over, GG."
      else
        turn <- turn + 1
        let startPos = ((int(move.[0])-97), (int(move.[1])-49))
        let endPos   = ((int(move.[3])-97), (int(move.[4])-49))
        board.move (startPos) (endPos)
        System.Console.Clear()
        makeNums()
        printfn "%A" board
