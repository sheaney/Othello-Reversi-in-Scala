case class Board(val board: Array[Array[Int]] = Array.fill(8,8)(0)) {
  require(board.length == 8 && board(0).length == 8)

  // Disk flipping checks
  private val upLeftDiagonalCheck = (i: Int, j: Int, disk: Int) => i >= 0 && j >= 0 && board(i)(j) == disk
  private val upRightDiagonalCheck = (i: Int, j: Int, disk: Int) => i >= 0 && j < 8 && board(i)(j) == disk
  private val downRightDiagonalCheck = (i: Int, j: Int, disk: Int) => i < 8 && j < 8 && board(i)(j) == disk
  private val downLeftDiagonalCheck = (i: Int, j: Int, disk: Int) => i < 8 && j >= 0 && board(i)(j) == disk
  private val leftCheck = (i: Int, j: Int, disk: Int) => j >= 0 && board(i)(j) == disk
  private val upCheck = (i: Int, j: Int, disk: Int) => i >= 0 && board(i)(j) == disk
  private val rightCheck = (i: Int, j: Int, disk: Int) => j < 8 && board(i)(j) == disk
  private val downCheck = (i: Int, j: Int, disk: Int) => i < 8 && board(i)(j) == disk

  // Directional movements
  private val left, up = (x: Int) => x - 1
  private val right, down = (x: Int) => x + 1 
  private val none = (x: Int) => x

  private def getPlayerDisk(i: Int, j: Int, dir: Int) = dir match {
    case 1 if i-1 >= 0 && j-1 >= 0 => board(i-1)(j-1)
    case 2 if i-1 >= 0 && j+1 < 8 => board(i-1)(j+1)
    case 3 if i+1 < 8 && j+1 < 8 => board(i+1)(j+1)
    case 4 if i+1 < 8 && j-1 >= 0 => board(i+1)(j-1)
    case 5 if j-1 >= 0 => board(i)(j-1) case 6 if i-1 >= 0 => board(i-1)(j)
    case 7 if j+1 < 8 => board(i)(j+1)
    case 8 if i+1 < 8 => board(i+1)(j)
    case _ => 0
  }

  def findPossibleMoves: (List[State], List[State]) = {
    var player1States = List[State]()
    var player2States = List[State]()

    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        if (board(i)(j) == 0) {
          for (dir <- 1 to 8) {
            val playerDisk = getPlayerDisk(i, j, dir)
            if (playerDisk == 1 && findMove(i, j, dir, 1))
              player1States ::= (new State(i, j, dir, 1))
            else if (playerDisk == 2 && findMove(i, j, dir , 2))
              player2States ::= (new State(i, j, dir , 2))
          }
        }
      }
    }
    // return a pair of possible moves for both players
    (player1States, player2States)
  }
   
  private def findDirectionMove(check: (Int, Int, Int) => Boolean, i: Int, dirI: Int => Int,
    j: Int, dirJ: Int => Int, currPlayer: Int): Boolean =
    if (check(dirI(i), dirJ(j), currPlayer))
      findDirectionMove(check, dirI(i), dirI, dirJ(j), dirJ, currPlayer)
    else if (check(dirI(i), dirJ(j), currPlayer % 2 + 1)) true
    else false

  // Method that will return 'true' if position is a possible move, 'false' otherwise 
  private def findMove(i: Int, j: Int, direction: Int, player: Int): Boolean = {
    direction match {
      case 1 => 
        findDirectionMove(upLeftDiagonalCheck, i, up, j, left, player)
      case 2 =>
        findDirectionMove(upRightDiagonalCheck, i, up, j, right, player)
      case 3 => 
        findDirectionMove(downRightDiagonalCheck, i, down, j, right, player)
      case 4 => 
        findDirectionMove(downLeftDiagonalCheck, i, down, j, left, player)
      case 5 =>
        findDirectionMove(leftCheck, i, none, j, left, player)
      case 6 =>
        findDirectionMove(upCheck, i, up, j, none, player)
      case 7 =>
        findDirectionMove(rightCheck, i, none, j, right, player)
      case 8 =>
        findDirectionMove(downCheck, i, down, j, none, player)
    }
  }

  private def updateBoardPositions(check: (Int, Int, Int) => Boolean, i: Int, dirI: Int => Int, 
    j: Int, dirJ: Int => Int, updatedDisk: Int) {
    if (check(dirI(i), dirJ(j), updatedDisk % 2 + 1)) {
      board(dirI(i))(dirJ(j)) = updatedDisk
      updateBoardPositions(check, dirI(i), dirI, dirJ(j), dirJ, updatedDisk)
    }
  }

  // update board according to the current players turn and move chosen
  def updateBoard(player: Player) {                                               
    val (currDisk, oppDisk) =
      player match {
        case _: Human => (2, 1)
        case _: Computer => (1, 2)
      }

    player.chosenMove foreach {
      state => {
        val (i, j) = (state.i, state.j)
                                                                                  
        board(i)(j) = currDisk 
        state.movement match {
          case 1 =>
            updateBoardPositions(upLeftDiagonalCheck, i, up, j, left, currDisk)
          case 2 =>
            updateBoardPositions(upRightDiagonalCheck, i, up, j, right, currDisk)
          case 3 =>
            updateBoardPositions(downRightDiagonalCheck, i, down, j, right, currDisk)
          case 4 =>
            updateBoardPositions(downLeftDiagonalCheck, i, down, j, left, currDisk)
          case 5 =>
            updateBoardPositions(leftCheck, i, none, j, left, currDisk)
          case 6 =>
            updateBoardPositions(upCheck, i, up, j, none, currDisk)
          case 7 =>
            updateBoardPositions(rightCheck, i, none, j, right, currDisk)
          case 8 =>
            updateBoardPositions(downCheck, i, down, j, none, currDisk)
        }
      }
    }
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

  // corner disks count as 3 additional disk to the total disk count
  def countCornerDisks(turn: Int): Int = {
    val disk = if (turn == 1) 2 else 1

    if (board(0)(0) == disk) 3 else 0 + {
      if (board(0)(7) == disk) 3 else 0 + {
        if (board(7)(0) == disk) 3 else 0 + {
          if (board(7)(7) == disk) 3 else 0
        }
      }
    }
  }

  def print() {
    println { strBoard }
  }

  private def strBoard: String = {
    def makeRow(row: Array[Int]): String = {
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

    val upperRow = (0 until board.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- board.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }

}

case class Accumulator(p1Disks: Int = 0, p2Disks: Int = 0)

object GameBoard extends Board {
  board(3)(3) = 1
  board(4)(4) = 1
  board(3)(4) = 2
  board(4)(3) = 2
}
