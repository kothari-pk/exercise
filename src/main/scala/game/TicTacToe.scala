package game

object TicTacToe {

  type Board = Array[Array[Player]]

  sealed trait Player
  case object X extends Player {
    override def toString: String = "X"
  }
  case object O extends Player {
    override def toString: String = "O"
  }
  case object Empty extends Player {
    override def toString: String = "_"
  }

  sealed trait Game {
    val board: Board
    var move: Player = X
    // row0, row1, row2, coll0, coll1, coll2, diag1, diag2
    val scoreX: Array[Int] = Array.fill[Int](8)(0)
    val scoreO: Array[Int] = Array.fill[Int](8)(0)
  }
  case class InProgress(board: Board) extends Game {
    def setNextMove: Unit = move = if(move == X) O else X
  }
  case class Finished(board: Board) extends Game
  case class InValid(board: Board) extends Game

  case class Position(row: Int, coll: Int)

  def createNewGame: Game = InProgress(Array.fill[Player](3, 3)(Empty))

  def printCurrentBoard(game: Game): String = game.board.map(_.mkString(" ")).mkString("\n")

  def playMove(game: Game, pos: Position, player: Player): Game = {
    // Make sure that correct player is making the move and the position is not already taken
    if (player != game.move && game.board(pos.row)(pos.coll) == Empty) {
      player match {
        case X =>
          game.scoreX(pos.row) += 1
          game.scoreX(pos.coll + 2) += 1
          if (pos.row == pos.coll) game.scoreX(2 * 3) += 1
          if (pos.row + pos.coll == 3) game.scoreX(2 * 3 + 1) += 1
        case O =>
          game.scoreO(pos.row) += 1
          game.scoreO(pos.coll + 2) += 1
          if (pos.row == pos.coll) game.scoreO(2 * 3) += 1
          if (pos.row + pos.coll == 3) game.scoreO(2 * 3 + 1) += 1
        case Empty => ()
      }
      val updatedGame = InProgress(game.board.updated(pos.row, game.board(pos.row).updated(pos.coll, player)))
      updatedGame.setNextMove
      updatedGame
    } else InValid(game.board)
  }

  def winner(game: Game): Option[Player] =
    if (game.scoreX.contains(3)) Some(X)
    else if(game.scoreO.contains(3)) Some(O)
    else None
}
