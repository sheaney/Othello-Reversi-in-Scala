case class Board(val board: Array[Array[Int]] = Array.fill(8,8)(0)) {
  require(board.length == 8 && board(0).length == 8)

  // Guards that check that the given indeces are within the board limits
  private val upLeftDiagonalCheck = (i: Int, j: Int) => i >= 0 && j >= 0
  private val upRightDiagonalCheck = (i: Int, j: Int) => i >= 0 && j < 8
  private val downRightDiagonalCheck = (i: Int, j: Int) => i < 8 && j < 8
  private val downLeftDiagonalCheck = (i: Int, j: Int) => i < 8 && j >= 0
  private val leftCheck = (i: Int, j: Int) => j >= 0
  private val upCheck = (i: Int, j: Int) => i >= 0
  private val rightCheck = (i: Int, j: Int) => j < 8
  private val downCheck = (i: Int, j: Int) => i < 8

  // Directional movements
  private val left, up = (x: Int) => x - 1
  private val right, down = (x: Int) => x + 1 
  private val none = (x: Int) => x

  // Verifies that the specified disk matches the one in the board
  private def isSameDisk(i: Int, j: Int, disk: Int) = board(i)(j) == disk 

  // Returns the opposite disk to the one indicated, available disks are 1 and 2
  private def opposite(disk: Int) = disk % 2 + 1

  private def getPlayerDisk(i: Int, j: Int, dir: Int) = dir match {
    case 1 if upLeftDiagonalCheck(up(i), left(j)) => 
      board(up(i))(left(j))
    case 2 if upRightDiagonalCheck(up(i), right(j)) => 
      board(up(i))(right(j))
    case 3 if downRightDiagonalCheck(down(i), right(j)) => 
      board(down(i))(right(j))
    case 4 if downLeftDiagonalCheck(down(i), left(j)) => 
      board(down(i))(left(j))
    case 5 if leftCheck(i, left(j)) => 
      board(i)(left(j)) 
    case 6 if upCheck(up(i), j) => 
      board(up(i))(j)
    case 7 if rightCheck(i, right(j)) => 
      board(i)(right(j))
    case 8 if downCheck(down(i), j) => 
      board(down(i))(j)
    case _ => 0
  }

  // Method that will return all the possible moves for each player, indicated by a list of
  // states that will help update the board accordingly. A move can have one or more corresponding
  // states indicated by a starting position (i, j) and the direction to guide the updating process
  def findPossibleMoves: (List[State], List[State]) = {
    var player1States = List[State]()
    var player2States = List[State]()

    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        if (board(i)(j) == 0) {
          for (dir <- 1 to 8) {
            val playerDisk = getPlayerDisk(i, j, dir)
            if (playerDisk == 1 && findMove(i, j, dir, 1))
              player1States ::= (new State(i, j, dir, player = 1))
            else if (playerDisk == 2 && findMove(i, j, dir , 2))
              player2States ::= (new State(i, j, dir , player = 2))
          }
        }
      }
    }
    // return a pair of possible moves for both players
    (player1States, player2States)
  }
  
  // Method that will check the availability of a move searching in a direction specified by dirI and dirJ 
  private def findDirectionalMove(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int,
    j: Int, dirJ: Int => Int, currPlayer: Int): Boolean =
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), currPlayer))
      findDirectionalMove(check, dirI(i), dirI, dirJ(j), dirJ, currPlayer)
    else if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(currPlayer))) true
    else false

  // Method that will return 'true' if position (i, j) is a possible move for the given player, 'false' otherwise 
  private def findMove(i: Int, j: Int, direction: Int, player: Int): Boolean = {
    direction match {
      case 1 => 
        findDirectionalMove(upLeftDiagonalCheck, i, up, j, left, player)
      case 2 =>
        findDirectionalMove(upRightDiagonalCheck, i, up, j, right, player)
      case 3 => 
        findDirectionalMove(downRightDiagonalCheck, i, down, j, right, player)
      case 4 => 
        findDirectionalMove(downLeftDiagonalCheck, i, down, j, left, player)
      case 5 =>
        findDirectionalMove(leftCheck, i, none, j, left, player)
      case 6 =>
        findDirectionalMove(upCheck, i, up, j, none, player)
      case 7 =>
        findDirectionalMove(rightCheck, i, none, j, right, player)
      case 8 =>
        findDirectionalMove(downCheck, i, down, j, none, player)
    }
  }

  // Method that will update the disks in a specified direction indicated by dirI and dirJ
  private def updateBoardPositions(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int, 
    j: Int, dirJ: Int => Int, updatedDisk: Int) {
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(updatedDisk))) {
      board(dirI(i))(dirJ(j)) = updatedDisk
      updateBoardPositions(check, dirI(i), dirI, dirJ(j), dirJ, updatedDisk)
    }
  }

  // update board according to the current players turn and the chosen move
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
        state.direction match {
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

  // Method that counts the total amount of disk on the board for both players
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

  // Corner disks count as 3 additional disks to the total disk count
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

  // Method that will print a string representation of the board to the terminal
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
