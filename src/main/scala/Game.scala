
object Game {

  var turnNo: Int = _
  var turn: Int = _ 
  var pingPong = false

  def main(args: Array[String]) {
    initializeTurns()
    gameLoop()
  }

  def initializeTurns() { turnNo = 4; turn = 1 }
  
  def gameLoop() {
    val gui = new GUI(GameBoard)
    gui.startGUI 

    while (turnNo < 64 && !pingPong) {
      val human = Human()
      val computer = Computer()
      val moves = GameBoard.findPossibleMoves 
      human.moves = moves._1
      computer.moves = moves._2
      
      turn match {
        case 1 =>
          if (human.canMove) takeTurn(human, GameBoard, turnNo)
          else pingPong = gui.cannotMove
        case 2 =>
          if (computer.canMove) takeTurn(computer, GameBoard, turnNo) 
          else gui.cannotMove
      }
    
      gui.update
      turn = turn % 2 + 1
    }
    
    obtainWinner match {
      case Some(Human()) =>
        gui.winner(Human())
      case Some(Computer()) =>
        gui.winner(Computer())
      case None =>
        gui.winner(None)
    }
    
  }

  def currentTurn = if (turn == 1) Human() else Computer()

  def takeTurn(player: Player, board: Board, turn: Int) {
    player.makeMove(board, turn)
    board.updateBoard(player)
    turnNo += 1
  }

  def obtainWinner: Option[Player] = {
    val result = GameBoard.countDisks
    val cmp = result.p1Disks compare result.p2Disks
    cmp match {
      case -1 => Some(Computer())
      case 0 => None 
      case 1 => Some(Human())
    }
  }
  
}
