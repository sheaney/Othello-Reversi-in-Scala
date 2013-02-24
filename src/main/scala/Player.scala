trait Player {

  type Move = List[State]
  var moves = List[Move]() // all available moves per turn
  var chosenMove = List[State]() // move with extra info on what disks to flip over

  def currentPlayer = this match {
    case _: Player1 => 1
    case _: Player2 => 2
  }

  def makeMove(b: Board, turnNo: Int): Unit
  def canMove: Boolean = !moves.isEmpty

  /** Method will return all possible moves that correspond to the states that
   * will help update the disks on the current board
   */
  def getPossibleMoves(currentBoard: Board): List[Move] =
    currentBoard.findPossibleMoves(currentPlayer)

  /**
   * This method will take a Reversi board and will update the
   * disks according to a some move
   */
  def simulateMove(currentBoard: Board, move: Move): Board = {
    val simulation = currentBoard.copy

    // Simulate a move and update board accordingly
    this.chosenMove = move
    simulation.updateBoard(this)
    
    simulation
  }

  /* Heursitic takes into account the difference of total 
   * disks + corner disks for Player2 and Player1 
   * for a simulated move
   */
  def evalHeuristic(board: Board, turn: Int): Int = {
    val result = board.countDisks
    
    if (turn == 64) result.p2Disks - result.p1Disks
    else {
      result.p2Disks + board.countCornerDisks(2) -
      result.p1Disks + board.countCornerDisks(1)
    }
  }

}


trait Human extends Player {
  
  override def makeMove(b: Board, turn: Int) {
    def promptMove: Move = {
      val (i, j) = GUI.awaitMoveSelection

      (moves.find { move =>
        move.head.i == i && move.head.j == j
      }) match {
        case Some(move) => move
        case None => promptMove
      }

    }
    chosenMove = promptMove
  }

}

trait Computer extends Player {

  override def makeMove(b: Board, turn: Int) {
    val board = b.copy
    val selectedMove = (AlphaBeta search (board, this, turn)).head

    chosenMove =
      (moves.find { move =>
        move.head.i == selectedMove.i && move.head.j == selectedMove.j
      }).get
  }

}

case class Player1() extends Player {
  def makeMove(b: Board, turn: Int) { }
}
case class Player2() extends Player {
  def makeMove(b: Board, turn: Int) { }
}
  
