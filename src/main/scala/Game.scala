import scala.math.random

object Game {

  var turnNo: Int = _
  var turn: Int = _ 

  def main(args: Array[String]) {
    initializeTurns()
    GameBoard.print()
    gameLoop()
  }

  def initializeTurns() { turnNo = 4; turn = 1 }
  
  def gameLoop() {
    val gui = new GUI(GameBoard)

    while (turnNo < 64) {
      val human = Human()
      val computer = Computer()
      val moves = GameBoard.findPossibleMoves 
      human.moves = moves._1
      computer.moves = moves._2
      
      turn match {
        case 1 =>
          println { "Player1's Turn" }
          human.printMoves()
          if (human.canMove) takeTurn(human, GameBoard, turnNo)
          else println { "You have nowhere to move" }
        case 2 =>
          println { "Player2's Turn" }
          computer.printMoves()
          if (computer.canMove) takeTurn(computer, GameBoard, turnNo) 
          else println { "Computer has nowhere to move" }
      }
      
      gui.update(GameBoard)
      turn = turn % 2 + 1
    }
    
    obtainWinner match {
      case Some(Human()) =>
        println { "Player 1 wins!" }
      case Some(Computer()) =>
        println { "Player 2 wins!" }
      case None =>
        println { "Tie!" }
    }
    
  }

  def currentTurn = if (turn == 1) Human() else Computer()

  def takeTurn(player: Player, board: Board, turn: Int) {
    player.makeMove(board, turn)
    board.updateBoard(player)
    turnNo += 1
    board.print()
  }

  def obtainWinner: Option[Player] = {
    val result = GameBoard.countDisks
    val cmp = result.p1Disks compare result.p2Disks
    println { "Player 1: "+ result.p1Disks +
              " Player 2: "+ result.p2Disks }
    cmp match {
      case -1 => Some(Computer())
      case 0 => None 
      case 1 => Some(Human())
    }
  }
  
}
