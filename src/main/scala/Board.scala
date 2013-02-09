class Board(protected val board: Array[Array[Int]] = Array.fill(8,8)(0)) extends Utilities {
  require(board.length == 8 && board(0).length == 8)

  type Move = List[State]

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

  /** 
   * Method that returns all the possible moves for a given player, indicated by a list of
   * states that will help update the board accordingly. A move can have one or more corresponding
   * states indicated by a starting position (i, j) and the direction to guide the updating process
   */
  def findPossibleMoves(playerDisk: Int): List[Move] =
    groupStatesByMove { 
      (for {
        i <- upperLimit to lowerLimit 
        j <- leftLimit to rightLimit
        if board(i)(j) == 0
        dir <- 1 to 8
        disk = getPlayerDisk(i, j, dir)
        if disk == playerDisk && findMove(i, j, dir)(playerDisk)
      } yield new State(i, j, dir, playerDisk)).toList
    }

  // Method that will check the availability of a move searching in a direction specified by dirI and dirJ 
  private def searchDirection(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int,
    j: Int, dirJ: Int => Int)(implicit currPlayer: Int): Boolean =
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), currPlayer))
      searchDirection(check, dirI(i), dirI, dirJ(j), dirJ)
    else if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(currPlayer))) true
    else false

  // Method that will return 'true' if position (i, j) is a possible move for the given player, 'false' otherwise 
  private def findMove(i: Int, j: Int, direction: Int)(implicit player: Int): Boolean = {
    direction match {
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

  /**
   * Method that will return a list of moves consisting of one or
   * more States that will help update the board. The size of the
   * list indicates how many moves the player has for a given turn
  */
  def groupStatesByMove(states: List[State]): List[Move] =
    if (!states.isEmpty)
      (List(states take 1) /: states.tail) {
        case (acc @ (lst @ hd :: _) :: tl, el) =>
          if (hd.i == el.i && hd.j == el.j)
            (el :: lst) :: tl
          else
            (el :: Nil) :: acc
        case x => x._1
      }
    else List[Move]() // empty list indicates that the player has no moves

  // Method that will update the disks in a specified direction indicated by dirI and dirJ
  private def updateBoardPositions(check: (Int, Int) => Boolean, i: Int, dirI: Int => Int, 
    j: Int, dirJ: Int => Int)(implicit updatedDisk: Int) {
    if (check(dirI(i), dirJ(j)) && isSameDisk(dirI(i), dirJ(j), opposite(updatedDisk))) {
      board(dirI(i))(dirJ(j)) = updatedDisk
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
                                                                                  
        board(i)(j) = updatedDisk 
        state.direction match {
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

    if (board(upperLimit)(leftLimit) == disk) 3 else 0 + {
      if (board(upperLimit)(rightLimit) == disk) 3 else 0 + {
        if (board(lowerLimit)(leftLimit) == disk) 3 else 0 + {
          if (board(lowerLimit)(rightLimit) == disk) 3 else 0
        }
      }
    }
  }

  def getDisk(i: Int, j: Int): Int = board(i)(j)

  def copy = new Board(board = this.board.map(_.clone))

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
      disksInRow mkString 
    }

    val upperRow = (0 until board.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- board.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }

}

object GameBoard extends Board {
  board(3)(3) = 1
  board(4)(4) = 1
  board(3)(4) = 2
  board(4)(3) = 2
}

