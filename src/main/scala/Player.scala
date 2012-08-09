import scala.math.random

abstract class Player(val turn: Int) {

  lazy val currentPlayer = this match {
    case Human() => Human()
    case Computer() => Computer()
  }

  var moves = List[State]() // all available moves per turn
  var chosenMove = List[State]() // move with extra info on what disks to flip over

  def makeMove(b: Board, turnNo: Int): Unit
  def canMove: Boolean = !moves.isEmpty
  def printMoves() = println(strMoves(turn, moves, ""))
  private def strMoves(turn: Int, moves: List[State],
    output: String): String = {
    moves match {
      case Nil => output
      case move :: moves => 
        strMoves(turn, moves,
          output +"Player "+ turn +": "+ move.x +" "+ move.y + "\n")
    }
  }

  def getPossibleMoves(currentBoard: Board): List[List[State]] = {
    val moves = this match {
      case Human() => currentBoard.findPossibleMoves._1
      case Computer() => currentBoard.findPossibleMoves._2
    }
    
    if (!moves.isEmpty) groupMoves(moves)
    else List[List[State]]()
  }

  def simulateMove(currentBoard: Board, move: List[State]): Board = {
    val player = currentPlayer
    val simulation = Board()
    simulation.board = currentBoard.board map (_.clone)

    // Simulate a move and update board accordingly
    player.chosenMove = move
    simulation.updateBoard(player)
    
    simulation
  }

  /* Heursitic takes into account the difference of total 
   * disks + corner disks for Player2 and Player1 
   * for a simulated move.
   */
  def evalHeuristic(board: Board): Int = {
    val result = board.countDisks

    (result.p2Disks + board.countCornerDisks(2)) -
    (result.p1Disks + board.countCornerDisks(1))
  }

  def groupMoves(moves: List[State]): List[List[State]] = {
    (List(moves take 1) /: moves.tail)
    {
      case (acc @ (lst @ hd :: _) :: tl, el) =>
        if (hd.x == el.x && hd.y == el.y)
          (el :: lst) :: tl
        else
          (el :: Nil) :: acc
      case x => x._1
    }
  }

}

case class Human() extends Player(1) {
  
  def makeMove(b: Board, turn: Int) {
    def promptMove: List[State] = {
      val (x, y) = GUI.awaitMoveSelection

      // find all states the board can be in for a given move
      moves.filter { move =>
        (x == move.x && y == move.y)
      } match {
        case Nil => promptMove
        case move => move
      }
    }

    chosenMove = promptMove
  }

}

case class Computer() extends Player(2) {

  def makeMove(b: Board, turn: Int) {
    val board = Board(b.board map (_.clone))
    val selectedMove = (AlphaBeta search (board, Computer(), turn)).head

    chosenMove = moves.filter { move =>
      (selectedMove.x == move.x && selectedMove.y == move.y)
    }
  }

}
