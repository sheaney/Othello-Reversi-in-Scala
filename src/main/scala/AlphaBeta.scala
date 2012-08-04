object AlphaBeta {

  // Debugging methods ---------------

  def getP(p: Player) = p.turn match {
    case 2 => "Computer"
    case 1 => "Human"
  }

  def printH(p: Player, d: Int, b: Board) {
    println("Player: "+ getP(p) +", Depth: "+ d +",  Heur: "+ p.evalHeuristic(b))
    println(b.strBoard)
  }

  def printR(v: Int, r: List[List[State]]) {
    println; println { "HEURISTIC ==> "+ v }
    r foreach { move =>
      println { "X: "+ move.head.x }
      println { "Y: "+ move.head.y }
      println
    }
  }

  // --------------------------------

  def not(p: Player) = p match {
    case Computer() => Human()
    case Human() => Computer()
  } 

  def max(x: (Int, List[List[State]]), y: (Int, List[List[State]])) = if (x._1 >= y._1) x else y
  def min(x: (Int, List[List[State]]), y: (Int, List[List[State]])) = if (x._1 <= y._1) x else y
  def terminal(turn: Int) = if (turn >= 64) true else false 

  def search(board: Board, player: Player, turn: Int): List[State] = {
    def alphaBeta(node: Board, depth: Int, a: Int, b: Int, r: List[List[State]], player: Player, turn: Int): (Int, List[List[State]]) = {
      var alpha = a
      var beta = b
      var moveChoice = r
      if (depth == 0 || terminal(turn)) {
        //println("Chosen Heuristic => "+ player.evalHeuristic(node))
        (player.evalHeuristic(node), r)
      }
      else {
        player match {
          // MAX PLAYER
          case Computer() => {
            player.getPossibleMoves(node).
              withFilter(_ => beta > alpha). // Pruning
                foreach { move =>
                  val simulate = player.simulateMove2(node, move)
                  //printH(player, depth, simulate)
                  val max1 = max((alpha, moveChoice), alphaBeta(simulate, depth-1, alpha, beta, move :: moveChoice, not(player), turn+1))
                  alpha = max1._1
                  moveChoice = max1._2
                }
            (alpha, moveChoice)
          }

          // MIN PLAYER
          case Human() => {
            player.getPossibleMoves(node).
              withFilter(_ => beta > alpha). // Pruning
                foreach { move =>
                  val simulate = player.simulateMove2(node, move)
                  //printH(player, depth, simulate)
                  val min1 = min((beta, moveChoice), alphaBeta(simulate, depth-1, alpha, beta, moveChoice, not(player), turn+1))
                  beta = min1._1
                }
            (beta, moveChoice)
          }
        }
      }
    }
    val (v, r) = alphaBeta(board, 5, Integer.MIN_VALUE, Integer.MAX_VALUE, List[List[State]](), player, turn)
    printR(v, r)
    r.head
  }

}
