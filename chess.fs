module Chess
type Color = White | Black
type Position = int * int

/// An abstract chess piece
[<AbstractClass>]
type chessPiece ( color : Color ) =
    let mutable _position : Position option = None
    abstract member nameOfType : string // " king " , " rook " , ...
    member this . color = color // White , Black
    member this . position // E . g . , (0 ,0) , (3 ,4) , etc .
        with get () = _position
        and set ( pos ) = _position <- pos
    override this . ToString () = // E . g . " K " for white king
        match color with
            White -> ( string this . nameOfType .[0]) . ToUpper ()
            | Black -> ( string this . nameOfType .[0]) . ToLower ()
    /// A list of runs , which is a list of relative movements ,e.g.,
    /// [[(1 ,0) ; (2 ,0) ;...]; [( -1 ,0) ; ( -2 ,0) ]...]. Runs must be
    /// ordered such that the first in a list is closest to the piece
    /// at hand .
    abstract member candiateRelativeMoves : Position list list
    // / Available moves and neighbours ([(1 ,0) ; (2 ,0) ;...] , [ p1 ;p2 ])
    member this.availableMoves ( board : Board ) : ( Position list * chessPiece list ) =
        board.getVacantNNeighbours this
    /// A board
    and Board () =
        let _array = Collections.Array2D.create <chessPiece option> 8
        8 None
    /// Wrap a position as option type
        let vali dPositio nWrap ( pos : Position ) : Position option =
            let ( rank , file ) = pos // square coordinate
            if rank < 0 || rank > 7 || file < 0 || file > 7
            then None
            else Some ( rank , file )
            /// Convert relative coordinates to absolute and remove
            /// out - of - board coordinates .
    let relativeToAbsolute ( pos : Position ) ( lst : Position list ) : Position list =
        let addPair ( a : int , b : int ) ( c : int , d : int ) : Position = ( a +c , b + d )
        // Add origin and delta positions
        List . map ( addPair pos ) lst
        // Choose absolute positions that are on the board
        | > List.choose validPositionWrap
    /// Board is indexed using .[ ,] notation
    member this . Item
        with get ( a : int , b : int ) = _array .[ a , b ]
        and set ( a : int , b : int ) ( p : chessPiece option ) =
            if p . IsSome then p . Value . position <- Some (a , b )
            _array .[ a , b ] <- p
    /// Produce string of board for , e.g., the printfn function.
    override this . ToString () =
        let rec boardStr ( i : int ) ( j : int ) : string =
            match (i , j ) with
                (8 ,0) -> " "
                | _ ->
                    let stripOption ( p : chessPiece option ) : string =
                        match p with
                            None -> " "
                            | Some p -> p . ToString ()
                // print top to bottom row
                let pieceStr = stripOption _array .[7 - i , j ]
                // let pieceStr = sprintf "(% d , % d ) " i j
                let lineSep = " " + String . replicate (8*4 -1) " -"
                match (i , j ) with
                (0 ,0) ->
                    let str = sprintf " % s \ n | %1 s " lineSep pieceStr
                    str + boardStr 0 1
                | (i ,7) ->
                    let str = sprintf " | %1 s |\ n % s \ n " pieceStr lineSep
                    str + boardStr ( i +1) 0
                | (i , j ) ->
                    let str = sprintf " | %1 s " pieceStr
                    str + boardStr i ( j +1)
        boardStr 0 0