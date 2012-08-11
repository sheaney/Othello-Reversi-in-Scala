case class Board(var board: Array[Array[Int]] = Array.fill(8,8)(0)) {
  require(board.length == 8 && board(0).length == 8)

	def findPossibleMoves: (List[State], List[State]) = {
    var player1States = List[State]()
    var player2States = List[State]()
    var i1 = 1; var j1 = 1; var dir = 1

    for ((row, i) <- board.zipWithIndex) {
      for ((cell, j) <- row.zipWithIndex) {

        if (cell == 0) {

          // upper left diagonal -> 1
          i1 = i-1; j1 = j-1; dir = 1
          if (i1 > 0 && j1 > 0) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // upper right diagonal -> 2
          i1 = i-1; j1 = j+1; dir = 2
          if (i1 > 0 && j1 < row.length) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // bottom right diagonal -> 3
          i1 = i+1; j1 = j+1; dir = 3
          if (i1 < board.length && j1 < row.length) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // bottom left diagonal -> 4
          i1 = i+1; j1 = j-1; dir = 4
          if (i1 < board.length && j1 > 0) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // left -> 5
          i1 = i; j1 = j-1; dir = 5
          if (j1 > 0) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // up -> 6
          i1 = i-1; j1 = j; dir = 6
          if (i1 > 0) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2)) //error?
              case _ =>
            }
          }
          // right -> 7
          i1 = i; j1 = j+1; dir = 7
          if (j1 < row.length) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }
          // down -> 8
          i1 = i+1; j1 = j; dir = 8
          if (i1 < board.length) {
            board(i1)(j1) match {
              case 1 => if (findMove(i1,j1,dir,1)) player1States ::= (new State(i,j,dir,1))
              case 2 => if (findMove(i1,j1,dir,2)) player2States ::= (new State(i,j,dir,2))
              case _ =>
            }
          }

        }
      }
    }
    // return a pair of possible moves for both players
    (player1States, player2States) 
  }

  // Method that will return 'true' if position is a possible move, 'false' otherwise
	private def findMove(i1: Int, j1: Int, direction: Int, player: Int): Boolean = {
    var flag = false
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
    
	def updateBoard(player: Player) {

    var friendlyDisk = 0
    var enemyDisk = 0
    var i1 = 1; var j1 = 1

    // set friendly and enemy disks according to current turn
    player match {
      case Human() =>
        friendlyDisk = 2
        enemyDisk = 1
      case Computer() =>
        friendlyDisk = 1
        enemyDisk = 2
    }
    
    player.chosenMove foreach {
      state => {
        val i = state.x
        val j = state.y

        board(i)(j) = friendlyDisk
        state.movement match {
          // upper left diagonal
          case 1 =>
            i1 = i-1; j1 = j-1
            while ((i1 > 0 && j1 > 0) && (board(i1)(j1) == enemyDisk)) {
              board(i1)(j1) = friendlyDisk
              i1 -= 1
              j1 -= 1
            }
          // upper right diagonal
          case 2 =>
            i1 = i-1; j1 = j+1
            while ((i1 > 0 && j1 < board(i).length) &&
              board(i1)(j1) == enemyDisk) {
              board(i1)(j1) = friendlyDisk
              i1 -= 1
              j1 += 1
            }
          // downward right diagonal
          case 3 =>
            i1 = i+1; j1 = j+1
            while ((i1 < board.length && j1 < board(i).length) &&
              board(i1)(j1) == enemyDisk) {
              board(i1)(j1) = friendlyDisk
              i1 += 1
              j1 += 1
            }
          // downard left diagonal
          case 4 =>
            i1 = i+1; j1 = j-1
            while ((i1 < board.length && j1 > 0) && board(i1)(j1) == enemyDisk) {
              board(i1)(j1) = friendlyDisk
              i1 += 1
              j1 -= 1
            }
          // left
          case 5 =>
            i1 = i; j1 = j-1
            while (j1 > 0 && (board(i1)(j1) == enemyDisk)) {
              board(i1)(j1) = friendlyDisk
              j1 -= 1
            }
          // up
          case 6 =>
            i1 = i-1; j1 = j
            while (i1 > 0 && (board(i1)(j1) == enemyDisk)) {
              board(i1)(j1) = friendlyDisk
              i1 -= 1
            }
          // right
          case 7 =>
            i1 = i; j1 = j+1
            while (j1 < board(i).length && (board(i1)(j1) == enemyDisk)) {
              board(i1)(j1) = friendlyDisk
              j1 += 1
            }
          // down
          case 8 =>
            i1 = i+1; j1 = j
            while (i1 < board.length && (board(i1)(j1) == enemyDisk)) {
              board(i1)(j1) = friendlyDisk
              i1 += 1
            }
        }
      }
    }
    board
  }

  def countDisks: Accumulator = {
    def aux(acc: Accumulator = Accumulator(), 
      b: List[Array[Int]] = board.toList): Accumulator = { b match {
        case Nil => acc
        case row :: rows =>
          val rowCount = disksInRow(row.toList)
          aux(acc.copy(p1Disks = acc.p1Disks + rowCount.p1Disks, 
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
    aux()
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
