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
  /// A list of runs, which is a list of relative movements ,e.g.,
  /// [[(1 ,0);(2,0);...];[(-1 ,0);(-2 ,0)]...]. Runs must be
  /// ordered such that the first in a list is closest to the piece 
  /// at hand.
  abstract member candidateRelativeMoves : Position list list
  /// Available moves and neighbours ([(1 ,0);(2 ,0);...],[p1;p2])
  member this.availableMoves (board:Board) : (Position list * chessPiece list) =
    let moves = board.getVacantNNeighbours this   // First part of the assignment
    if this.nameOfType.ToLower() = "king" then
      let mutable notSafeMoves = []
      for i in 0..7 do
        for j in 0..7 do
          let mutable (p:chessPiece option) = board.Item(i,j)
          if p.IsSome && this.color <> p.Value.color then
            if p.Value.nameOfType.ToLower() = "rook" then
              notSafeMoves <- 
                List.append notSafeMoves [for i in 0..7 -> (fst p.Value.position.Value, i)]
              notSafeMoves <- 
                List.append notSafeMoves [for i in 0..7 -> (i, snd p.Value.position.Value)]
      
            elif p.Value.nameOfType.ToLower() = "king" then
              for i in -1..1 do 
                for j in -1..1 do
                  notSafeMoves <- 
                    List.append notSafeMoves [((fst p.Value.position.Value)+i, 
                                               (snd p.Value.position.Value)+j)]       
      
      if (notSafeMoves.IsEmpty) then 
        board.getVacantNNeighbours this
      
      else 
        let safeMoves = fst moves |> List.filter (fun x -> not(List.contains x notSafeMoves))
        (safeMoves,(snd moves)) 
    else moves

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
          |> List.choose snd
        (vacant, opponent)


let checkInput (s: string) : (Position * Position)  = 
  let str = s.ToLower()
  let p = str.Split ' '
  let r = Regex.Match(str, @"[a-h][1-8]\s[a-h][1-8]")
  if str = "quit" then
    (-1,-1), (-1,-1) ///quitting sequence
  else
    if r.Success then
      ///example: converting "a4 a5" to ((0,4),(0,5))
      ((int(p.[0].[0])-97, (int(p.[0].[1]))-49), (int(p.[1].[0])-97 ,(int(p.[1].[1]))-49))
    else
      (100, 100), (100, 100) ///MOCKUP

[<AbstractClass>]
type Player (c: Color) =
  member this.color = c
  abstract member nameOfType : string
  abstract member nextMove : string -> Board -> string 

type Human (c: Color) =
  inherit Player(c)
  override this.nameOfType = string(this.color)
  override this.nextMove (s:string) (board:Board): string =
    ///input is converted using aux function checkInput, ex. from "a4 a5" to "(0,4),(0,5)"
    let check = checkInput s
    let source = fst (check)
    let target = snd (check)
    let mutable (p: chessPiece option) = board.Item(fst source, snd source)
    ///getting available moves for that specific piece.
    let moveList = fst (p.Value.availableMoves(board))
    if check = ((-1,-1), (-1,-1)) then ///quitting sequence string to be used in the game class
      "quit"  
    elif check = ((100, 100), (100, 100)) then ///wrong input sequence to be used in the game class
      "wrong"
    elif (p.IsSome) && (p.Value.color = this.color) && (List.contains target moveList) then 
          s
    else "wrong" ///MOCKUP

type Game (player1 : Player, player2: Player) =
  member this.p1 = player1
  member this.p2 = player2  
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

      printfn "%s's move?" cPlayer.nameOfType

      codeString <- System.Console.ReadLine()
      let pos = checkInput codeString

      /// From here --- Needs fixing
      if ( pos = ((-1,-1), (-1,-1)) ) then
        codeString <- "quit"
        printfn "Game over, GG."
      elif (pos = ((100, 100), (100, 100))) then
        printfn "Try again!"
      else
        codeString <- "wrong"
        turn <- turn + 1
        board.move (fst pos) (snd pos)
        makeNums()
        printfn "%A" board
      /// To here