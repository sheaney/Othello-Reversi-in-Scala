object Game {

  val human = Human()
  val computer = Computer()
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
      
      turn match {
        case 1 =>
          human.moves = GameBoard.findPossibleMoves(turn)
          if (human.canMove) takeTurn(human, GameBoard, turnNo)
          else pingPong = gui.cannotMove(turnNo)
        case 2 =>
          computer.moves = GameBoard.findPossibleMoves(turn)
          if (computer.canMove) takeTurn(computer, GameBoard, turnNo) 
          else gui.cannotMove(turnNo)
      }
    
      gui.update(turn)
      turn = turn % 2 + 1
    }

    obtainWinner match {
      case Some(_: Human) =>
        gui.winner(human)
        case Some(_: Computer) =>
        gui.winner(computer)
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
      case -1 => Some(computer)
      case 0 => None 
      case 1 => Some(human)
    }
  }
  
}

