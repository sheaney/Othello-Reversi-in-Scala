import scala.math.random

abstract class Player(val turn: Int) {

  lazy val currentPlayer = this match {
    case Human() => Human()
    case Computer() => Computer()
  }

  var moves = List[State]() // all available moves
  var chosenMove = List[State]() // move with extra info on what disks to flip over

  def makeMove(b: Board, turnNo: Int): Unit
  def canMove: Boolean = !moves.isEmpty
  def printMoves() = println { strMoves(turn, moves, "") }
  def strMoves(turn: Int, moves: List[State],
    output: String): String = {
    moves match {
      case Nil => output
      case move :: moves => 
        strMoves(turn, moves,
          output +"Player "+ turn +": "+ move.x +" "+ move.y + "\n")
    }
  }
  
  def simulateMove: (Int, Board) = {
    val moves = GameBoard.findPossibleMoves._2
    var heuristicBoards = List[(Int, Board)]()
    
    groupMoves(moves) foreach { move =>
      val simulation = Board()
      val b = GameBoard.board.map(_.clone)
      simulation.board = b

      val player = this match {
        case Human() => Human()
        case Computer() => Computer()
      }

      player.chosenMove = move
      simulation.updateBoard(player)
      println { "Sim utility: "+ evalHeuristic(simulation) }
      simulation.print()
      heuristicBoards ::= (evalHeuristic(simulation), simulation)
    }
    
    (heuristicBoards.head /: heuristicBoards.tail) {
      case ((x1, x2), (y1, y2)) =>
        if (x1 > y1) (x1, x2)
        else (y1, y2)
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

  def simulateMove2(currentBoard: Board, move: List[State]): Board = {
    val player = currentPlayer
    val simulation = Board()
    val b = currentBoard.board map (_.clone)
    simulation.board = b

    // Simulate a move and update board accordingly
    player.chosenMove = move
    simulation.updateBoard(player)
    
    simulation
  }

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
      try {
        print { ">> X: " }
        val x = readInt
        print { ">> Y: " }
        val y = readInt
     
        // find all states the board can be in for a given move
        moves.filter { move =>
          (x == move.x && y == move.y)
        } match {
          case Nil => promptMove
          case move => move
        }
      } catch {
          case _ => promptMove
      }
    }
    chosenMove = promptMove
  }

}

case class Computer() extends Player(2) {

  def makeMove2() {
    def calculateMove() {
      val selectedMove = moves((random * moves.size).toInt)

      println { "("+ selectedMove.x +","+ selectedMove.y +")" }

      for (move <- moves) {
        if (selectedMove.x == move.x && selectedMove.y == move.y)
          chosenMove ::= move
      }
    }
    calculateMove()
  }
  
  def makeMove(b: Board, turn: Int) {
    val board = Board(b.board map (_.clone))
    val selectedMove = (AlphaBeta search (board, Computer(), turn)).head

    println { "("+ selectedMove.x +","+ selectedMove.y +")" }
    
    chosenMove = moves.filter { move =>
      (selectedMove.x == move.x && selectedMove.y == move.y)
    }
  }

}
