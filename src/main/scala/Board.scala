import annotation.switch

class Board(val repr: Array[Array[Int]] = Array.fill(8,8)(0)) extends Utilities {
  require(repr.length == 8 && repr(0).length == 8)

  type Move = IndexedSeq[State]

  // Verifies that the specified disk matches the one in the board
  private def isSameDisk(i: Int, j: Int, disk: Int) = repr(i)(j) == disk 

  // Returns the opposite disk to the one indicated, available disks are 1 and 2
  private def opposite(disk: Int) = disk % 2 + 1

  private def getPlayerDisk(i: Int, j: Int, dir: Int) = dir match {
    case 1 if upLeftDiagonalCheck(up(i), left(j)) => 
      repr(up(i))(left(j))
    case 2 if upRightDiagonalCheck(up(i), right(j)) => 
      repr(up(i))(right(j))
    case 3 if downRightDiagonalCheck(down(i), right(j)) => 
      repr(down(i))(right(j))
    case 4 if downLeftDiagonalCheck(down(i), left(j)) => 
      repr(down(i))(left(j))
    case 5 if leftCheck(i, left(j)) => 
      repr(i)(left(j)) 
    case 6 if upCheck(up(i), j) => 
      repr(up(i))(j)
    case 7 if rightCheck(i, right(j)) => 
      repr(i)(right(j))
    case 8 if downCheck(down(i), j) => 
      repr(down(i))(j)
    case _ => 0
  }

  private def exists(move: Move): Boolean = !move.isEmpty

  /**
   * Method that returns an Sequence of zero or more states that will correspond
   * to zero or one move and that will contain the directions (i, j) to update
   * the board
   */
  private def generateMove(i: Int, j: Int, playerDisk: Int): IndexedSeq[State] =
    for {
      dir <- 1 to 8
      disk = getPlayerDisk(i, j, dir)
      if disk == playerDisk && findMove(i, j, dir)(playerDisk)
    } yield new State(i, j, dir, playerDisk)

  /**
   * Method that returns a Stream of all the possible moves for a given player
   * A move can have one or more corresponding states indicated by a starting
   * position (i, j) and the direction to guide the updating process
   */
  def findPossibleMoves(playerDisk: Int): Stream[Move] =
    for {
      i <- (upperLimit to lowerLimit).toStream
      j <- (leftLimit to rightLimit).toStream
      if repr(i)(j) == 0
      move = generateMove(i, j, playerDisk)
      if exists(move)
    } yield move

  // Method that will check the availability of a move searching in a direction specified by dirI and dirJ 
  private def searchDirection(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int,
    j: Int, dirJ: Int => Int)(implicit currPlayer: Int): Boolean =
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), currPlayer))
      searchDirection(check, dirI(i), dirI, dirJ(j), dirJ)
    else if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(currPlayer))) true
    else false

  // Method that will return 'true' if position (i, j) is a possible move for the given player, 'false' otherwise 
  private def findMove(i: Int, j: Int, direction: Int)(implicit player: Int): Boolean = {
    (direction: @switch) match {
      case 1 => 
        searchDirection(upLeftDiagonalCheck, i, up, j, left)
      case 2 =>
        searchDirection(upRightDiagonalCheck, i, up, j, right)
      case 3 => 
        searchDirection(downRightDiagonalCheck, i, down, j, right)
      case 4 => 
        searchDirection(downLeftDiagonalCheck, i, down, j, left)
      case 5 =>
        searchDirection(leftCheck, i, none, j, left)
      case 6 =>
        searchDirection(upCheck, i, up, j, none)
      case 7 =>
        searchDirection(rightCheck, i, none, j, right)
      case 8 =>
        searchDirection(downCheck, i, down, j, none)
    }
  }

  // Method that will update the disks in a specified direction indicated by dirI and dirJ
  private def updateBoardPositions(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int, 
    j: Int, dirJ: Int => Int)(implicit updatedDisk: Int) {
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(updatedDisk))) {
      repr(dirI(i))(dirJ(j)) = updatedDisk
      updateBoardPositions(check, dirI(i), dirI, dirJ(j), dirJ)
    }
  }

  // update board according to the current players turn and the chosen move
  def updateBoard(player: Player) {                                               
    implicit val updatedDisk =
      player match {
        case _: Player1 => 2
        case _: Player2 => 1
      }

    player.chosenMove foreach {
      state => {
        val (i, j) = (state.i, state.j)
                                                                                  
        repr(i)(j) = updatedDisk 
        (state.direction: @switch) match {
          case 1 =>
            updateBoardPositions(upLeftDiagonalCheck, i, up, j, left)
          case 2 =>
            updateBoardPositions(upRightDiagonalCheck, i, up, j, right)
          case 3 =>
            updateBoardPositions(downRightDiagonalCheck, i, down, j, right)
          case 4 =>
            updateBoardPositions(downLeftDiagonalCheck, i, down, j, left)
          case 5 =>
            updateBoardPositions(leftCheck, i, none, j, left)
          case 6 =>
            updateBoardPositions(upCheck, i, up, j, none)
          case 7 =>
            updateBoardPositions(rightCheck, i, none, j, right)
          case 8 =>
            updateBoardPositions(downCheck, i, down, j, none)
        }
      }
    }
  }

  // Method that counts the total amount of disk on the board for both players
  def countDisks: Accumulator = {
    def countRows(acc: Accumulator = Accumulator()): Accumulator =
      (Accumulator() /: this.repr){ (acc, row) =>
        val rowCount = disksInRow(row.toList)
        acc.copy(
          p1Disks = acc.p1Disks + rowCount.p1Disks,
          p2Disks = acc.p2Disks + rowCount.p2Disks
        )
      }
    
    def disksInRow(row: List[Int]): Accumulator =
      (Accumulator() /: row)((acc, disk) =>
        (disk: @switch) match {
          case 2 => acc.copy(p1Disks = acc.p1Disks + 1)
          case 1 => acc.copy(p2Disks = acc.p2Disks + 1)
          case _ => acc
        }
      )                                              
    countRows()
  }

  // Corner disks count as 3 additional disks to the total disk count
  def countCornerDisks(turn: Int): Int = {
    val disk = if (turn == 1) 2 else 1

    if (repr(upperLimit)(leftLimit) == disk) 3 else 0 + {
      if (repr(upperLimit)(rightLimit) == disk) 3 else 0 + {
        if (repr(lowerLimit)(leftLimit) == disk) 3 else 0 + {
          if (repr(lowerLimit)(rightLimit) == disk) 3 else 0
        }
      }
    }
  }

  def getDisk(i: Int, j: Int): Int = repr(i)(j)

  def copy = new Board(repr = this.repr.map(_.clone))

  // Method that gives a string representation of the board
  override def toString = strBoard 

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
      disksInRow.mkString 
    }

    val upperRow = (0 until repr.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- repr.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }

}

object GameBoard extends Board {
  repr(3)(3) = 1
  repr(4)(4) = 1
  repr(3)(4) = 2
  repr(4)(3) = 2
}
