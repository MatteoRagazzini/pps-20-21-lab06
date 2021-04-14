package u06lab.code


object TicTacToe  extends  App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(m => m.x == x && m.y == y).map(m=> m.player)

//  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
//    var solutions: Seq[Board] = Seq()
//    for(x <- 0 to 2; y <- 0 to 2) {
//        // if in a certain cell there isn't a mark
//        if (find(board, x, y).isEmpty) {
//          // Append to solutions a new board with a new mark in the free position
//          solutions = solutions :+ (board :+ Mark(x,y,player))
//        }
//    }
//    solutions
//  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for(x <- 0 to 2;
        y <- 0 to 2
        if find(board,x,y).isEmpty)
    yield board:+Mark(x,y,player)
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ =>  for {
      // the container type is determined by the first generator type -> Stream
      g <- computeAnyGame(player.other, moves - 1);
      b <- placeAnyMark(g.head, player)
      // because we said yield game to the Stream we return a Stream[Game]
    } yield b::g
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  var boards: Seq[Board] = placeAnyMark(List(), X)
  println(boards.size)
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4) foreach {g => printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}
