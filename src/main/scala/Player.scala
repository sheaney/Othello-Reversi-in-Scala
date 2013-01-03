trait Player {

  var moves = List[State]() // all available moves per turn
  var chosenMove = List[State]() // move with extra info on what disks to flip over

  def currentPlayer = this match {
    case _: Human => 1
    case _: Computer => 2
  }

  def makeMove(b: Board, turnNo: Int): Unit
  def canMove: Boolean = !moves.isEmpty
  def printMoves() = println(strMoves(moves, ""))
  private def strMoves(moves: List[State], output: String): String = {
    moves match {
      case Nil => output
      case move :: moves => 
        strMoves(moves,
          output +"Player "+ this.toString +": "+ move.i +" "+ move.j + "\n")
    }
  }


  /** Method will return all possible moves that correspond to the states that
   * will help update the disks on the current board
   */
  def getPossibleMoves(currentBoard: Board): List[List[State]] =
    groupStatesByMove(currentBoard.findPossibleMoves(currentPlayer))

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

  /**
   * Method that will return a list of one or more states that represents
   * the number of directions the board will need to update disks for a given
   * move. The length of the result list will be the number of available
   * moves for the player
  */
  def groupStatesByMove(states: List[State]): List[List[State]] =
    if (!states.isEmpty) {
      (List(states take 1) /: states.tail)
      {
        case (acc @ (lst @ hd :: _) :: tl, el) =>
          if (hd.i == el.i && hd.j == el.j)
            (el :: lst) :: tl
          else
            (el :: Nil) :: acc
        case x => x._1
      }
    } else List[List[State]]()

}

case class Human() extends Player {
  
  def makeMove(b: Board, turn: Int) {
    def promptMove: List[State] = {
      val (i, j) = GUI.awaitMoveSelection

      // find all states the board can be in for a given move
      moves.filter { move =>
        (i == move.i && j == move.j)
      } match {
        case Nil => promptMove
        case move => move
      }
    }

    chosenMove = promptMove
  }

}

case class Computer() extends Player {

  def makeMove(b: Board, turn: Int) {
    val board = Board(b.board map (_.clone))
    val selectedMove = (AlphaBeta search (board, Computer(), turn)).head

    chosenMove = moves.filter { move =>
      (selectedMove.i == move.i && selectedMove.j == move.j)
    }
  }

}
