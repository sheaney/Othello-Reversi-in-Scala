object Game {

  val player1 = new Player1 with Human
  val player2 = new Player2 with Computer
  /* Can also play two People against each other like this:
  val player1 = new Player1 with Computer
  val player2 = new Player2 with Human */
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
          player1.moves = GameBoard.findPossibleMoves(turn)
          if (player1.canMove) takeTurn(player1, GameBoard, turnNo)
          else pingPong = gui.cannotMove(turnNo)
        case 2 =>
          player2.moves = GameBoard.findPossibleMoves(turn)
          if (player2.canMove) takeTurn(player2, GameBoard, turnNo) 
          else gui.cannotMove(turnNo)
      }
    
      gui.update(turn)
      turn = turn % 2 + 1
    }

    gui.setWinner(obtainWinner)
  }

  def currentTurn = if (turn == 1) Player1() else Player2()

  def takeTurn(player: Player, board: Board, turn: Int) {
    player.makeMove(board, turn)
    board.updateBoard(player)
    turnNo += 1
  }

  def obtainWinner: Option[Player] = {
    val result = GameBoard.countDisks
    result.p1Disks compare result.p2Disks match {
      case -1 => Some(player2)
      case 0 => None
      case 1 => Some(player1)
    }
  }
  
}
