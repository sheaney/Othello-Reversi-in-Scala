case class Board(var board: Array[Array[Int]] = Array.fill(8,8)(0)) {
  require(board.length == 8 && board(0).length == 8)

  private def getPlayerDisk(i: Int, j: Int, dir: Int) = dir match {
    case 1 if i-1 >= 0 && j-1 >= 0 => board(i-1)(j-1)
    case 2 if i-1 >= 0 && j+1 < 8 => board(i-1)(j+1)
    case 3 if i+1 < 8 && j+1 < 8 => board(i+1)(j+1)
    case 4 if i+1 < 8 && j-1 >= 0 => board(i+1)(j-1)
    case 5 if j-1 >= 0 => board(i)(j-1)
    case 6 if i-1 >= 0 => board(i-1)(j)
    case 7 if j+1 < 8 => board(i)(j+1)
    case 8 if i+1 < 8 => board(i+1)(j)
    case _ => 0
  }
   
  def findPossibleMoves: (List[State], List[State]) = {
    var player1States = List[State]()
    var player2States = List[State]()
    var i1 = 1; var j1 = 1; var dir = 1
                                            
    for ((row, i) <- board.zipWithIndex) {
      for ((cell, j) <- row.zipWithIndex) {
                                            
        if (cell == 0) {
          for (dir <- 1 to 8) {
            val playerDisk = getPlayerDisk(i, j, dir)
            if (playerDisk == 1 && findMove(i, j, dir, 1))
              player1States ::= (new State(i, j, dir, 1))
            else if (playerDisk == 2 && findMove(i, j, dir, 2))
              player2States ::= (new State(i, j, dir, 2))
          }
        }
      }
    }
    // return a pair of possible moves for both players
    (player1States, player2States)
  }

  // Method that will return 'true' if position is a possible move, 'false' otherwise
	private def findMove(i1: Int, j1: Int, direction: Int, player: Int): Boolean = {
    var i = i1; var j = j1
    direction match {
      case 1 => 
        i -= 1; j -= 1
        while ((i > 0 && j > 0) && board(i)(j) == player) {
          i -= 1
          j -= 1
        }
        if ((i >= 0 && j >= 0) && board(i)(j) == (player%2+1)) true 
        else false
      case 2 => 
        i -= 1; j += 1
        while ((i > 0 && j < board(i).length) && 
          board(i)(j) == player) {
          i -= 1
          j += 1
        }
        if ((i >= 0 && j < board(i).length) &&
          board(i)(j) == (player%2+1))
          true
        else false
      case 3 => 
        i += 1; j += 1
        while ((i < board.length && j < board(i).length) &&
          board(i)(j) == player) {
          i += 1
          j += 1
        }
        if ((i < board.length && j < board(i).length) &&
          board(i)(j) == (player%2+1))
          true
        else false
        case 4 => 
          i += 1; j -= 1
          while ((i < board.length && j > 0) &&
            board(i)(j) == player) {
            i += 1
            j -= 1
          }
          if ((i < board.length && j >= 0) &&
            board(i)(j) == (player%2+1))
            true
          else false
        case 5 =>
          j -= 1
          while (j > 0 && (board(i)(j) == player)) j -= 1
          if (j >= 0 && (board(i)(j) == (player%2+1))) true
          else false
        case 6 =>
          i -= 1
          while (i > 0 && (board(i)(j) == player)) i -= 1
          if (i >= 0 && (board(i)(j) == (player%2+1))) true
          else false
        case 7 =>
          j += 1
          while (j < board(i).length && (board(i)(j) == player)) j += 1
          if (j < board(i).length && (board(i)(j) == (player%2+1))) true
          else false
        case 8 =>
          i += 1
          while (i < board.length && (board(i)(j) == player)) i += 1
          if (i < board.length && (board(i)(j) == (player%2+1))) true
          else false
    }
  }

  private def updateBoardPositions(cond: (Int, Int) => Boolean, i: Int, dirI: Int => Int, 
    j: Int, dirJ: Int => Int, updatedDisk: Int) {
    var i1 = dirI(i)
    var j1 = dirJ(j)
    while (cond(i1, j1)) {
      board(i1)(j1) = updatedDisk
      i1 = dirI(i1)
      j1 = dirJ(j1)
    }
  }

  def updateBoard(player: Player) {                                               
                                                                                  
    // set friendly and enemy disks according to current turn
    val (currDisk, oppDisk) =
      player match {
        case _: Human => (2, 1)
        case _: Computer => (1, 2)
      }

    player.chosenMove foreach {
      state => {
        val i = state.x
        val j = state.y
                                                                                  
        board(i)(j) = currDisk 
        state.movement match {
          // upper left diagonal
          case 1 =>
            updateBoardPositions((i1,j1) => i1>0 && j1>0 && board(i1)(j1) == oppDisk,
              i, _ - 1, j, _ - 1, currDisk)
          // upper right diagonal
          case 2 =>
            updateBoardPositions((i1,j1) => i1>0 && j1<8 && board(i1)(j1) == oppDisk,
              i, _ - 1, j, _ + 1, currDisk)
          // downward right diagonal
          case 3 =>
            updateBoardPositions((i1,j1) => i1<8 && j1<8 && board(i1)(j1) == oppDisk,
              i, _ + 1, j, _ + 1, currDisk)
          // downard left diagonal
          case 4 =>
            updateBoardPositions((i1,j1) => i1<8 && j1>0 && board(i1)(j1) == oppDisk,
              i, _ + 1, j, _ - 1, currDisk)
          // left
          case 5 =>
            updateBoardPositions((i1,j1) => j1>0 && board(i1)(j1) == oppDisk,
              i, x => x, j, _ - 1, currDisk)
          // up
          case 6 =>
            updateBoardPositions((i1,j1) => i1>0 && board(i1)(j1) == oppDisk,
              i, _ - 1, j, x => x, currDisk)
          // right
          case 7 =>
            updateBoardPositions((i1,j1) => j1<8 && board(i1)(j1) == oppDisk,
              i, x => x, j, _ + 1, currDisk)
          // down
          case 8 =>
            updateBoardPositions((i1,j1) => i1<8 && board(i1)(j1) == oppDisk,
              i, _ + 1, j, x => x, currDisk)
        }
      }
    }
    board
  }                                                                                

  def countDisks: Accumulator = {
    def countRows(acc: Accumulator = Accumulator(), 
      b: List[Array[Int]] = board.toList): Accumulator = { b match {
        case Nil => acc
        case row :: rows =>
          val rowCount = disksInRow(row.toList)
          countRows(acc.copy(p1Disks = acc.p1Disks + rowCount.p1Disks, 
                       p2Disks = acc.p2Disks + rowCount.p2Disks), rows)
      }
    }
    
    def disksInRow(row: List[Int]): Accumulator = {
      (Accumulator() /: row)((acc, disk) =>
        disk match {
          case 2 => acc.copy(p1Disks = acc.p1Disks + 1)
          case 1 => acc.copy(p2Disks = acc.p2Disks + 1)
          case _ => acc
        })                                              
    }
    countRows()
  }

  def countCornerDisks(turn: Int): Int = {
    var acc = 0
    var disk = 0

    if (turn == 1)
      disk = 2
    else
      disk = 1

    if (board(0)(0) == disk)
      acc += 3 
    if (board(0)(board.length - 1) == disk)
      acc += 3
    if (board(board(0).length - 1)(0) == disk)
      acc += 3
    if (board(board(0).length - 1)(board.length - 1) == disk)
      acc += 3

    acc
  }

  def print() {
    println { strBoard }
  }

  private def strBoard: String = {
    val upperRow = (0 until board.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- board.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }

  private def makeRow(row: Array[Int]): String = {
    val disksInRow = 
      for (cell <- row) yield {
        cell match {
          case 0 => " - "
          case 1 => " X "
          case 2 => " O "
        }
      }
    disksInRow mkString 
  }

}

case class Accumulator(p1Disks: Int = 0, p2Disks: Int = 0)

object GameBoard extends Board {
  board(3)(3) = 1
  board(4)(4) = 1
  board(3)(4) = 2
  board(4)(3) = 2
}
