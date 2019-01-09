open Chess
open Pieces
/// Print various information about a piece
let printPiece ( board : Board ) ( p : chessPiece ) : unit =
  printfn "%A : %A %A" p p.position ( p.availableMoves board )
// Create a game
let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king ( White ) :> chessPiece ;
  rook ( White ) :> chessPiece ;
  king ( Black ) :> chessPiece ;
  rook ( White ) :> chessPiece |]
// Place pieces on the board
board.[0 ,0] <- Some pieces.[0]
board.[1 ,1] <- Some pieces.[1]
board.[4 ,1] <- Some pieces.[2]
board.[2 ,2] <- Some pieces.[3]


//Running the game
let p1 = Chess.Human(White)
let p2 = Chess.Human(Black)
let game = Chess.Game (p1,p2)
game.run(board)
