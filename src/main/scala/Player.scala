trait Player {

  var moves = List[List[State]]() // all available moves per turn
  var chosenMove = List[State]() // move with extra info on what disks to flip over

  def currentPlayer = this match {
    case _: Human => 1
    case _: Computer => 2
  }

  def makeMove(b: Board, turnNo: Int): Unit
  def canMove: Boolean = !moves.isEmpty

  /** Method will return all possible moves that correspond to the states that
   * will help update the disks on the current board
   */
  def getPossibleMoves(currentBoard: Board): List[List[State]] =
    currentBoard.findPossibleMoves(currentPlayer)

  /**
   * This method will take a Reversi board and will update the
   * disks according to a some move
   */
  def simulateMove(currentBoard: Board, move: List[State]): Board = {
    val simulation = Board(board = currentBoard.board map (_.clone))

    // Simulate a move and update board accordingly
    this.chosenMove = move
    simulation.updateBoard(this)
    
    simulation
  }

  /* Heursitic takes into account the difference of total 
   * disks + corner disks for Player2 and Player1 
   * for a simulated move
   */
  def evalHeuristic(board: Board): Int = {
    val result = board.countDisks

    (result.p2Disks + board.countCornerDisks(2)) -
    (result.p1Disks + board.countCornerDisks(1))
  }

}

case class Human() extends Player {
  
  def makeMove(b: Board, turn: Int) {
    def promptMove: List[State] = {
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

case class Computer() extends Player {

  def makeMove(b: Board, turn: Int) {
    val board = Board(b.board map (_.clone))
    val selectedMove = (AlphaBeta search (board, Computer(), turn)).head

    chosenMove =
      (moves.find { move =>
        move.head.i == selectedMove.i && move.head.j == selectedMove.j
      }).get
  }

}
