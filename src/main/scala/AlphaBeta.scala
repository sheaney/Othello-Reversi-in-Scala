object AlphaBeta {

  type Move = List[State]
  // Debugging methods ---------------

  def getP(p: Player) = p match {
    case _: Player1 => "Computer"
    case _: Player2 => "Human"
  }

  def printH(p: Player, d: Int, b: Board) {
    println("Player: "+ getP(p) +", Depth: "+ d +",  Heur: "+ p.evalHeuristic(b))
    println(b)
  }

  def printR(v: Int, r: List[Move]) {
    println; println { "HEURISTIC ==> "+ v }
    r foreach { move =>
      println { "i: "+ move.head.i }
      println { "j: "+ move.head.j }
      println
    }
  }

  // --------------------------------

  def not(p: Player) = p match {
    case _: Player2 => Player1()
    case _: Player1 => Player2()
  } 

  def max(x: (Int, List[Move]), y: (Int, List[Move])) = if (x._1 >= y._1) x else y
  def min(x: (Int, List[Move]), y: (Int, List[Move])) = if (x._1 <= y._1) x else y
  def terminal(turn: Int) = if (turn >= 64) true else false 

  def search(board: Board, player: Player, turn: Int): Move = {
    def alphaBeta(node: Board, depth: Int, a: Int, b: Int, r: List[Move], player: Player, turn: Int): (Int, List[Move]) = {
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
          case _: Player2 => {
            player.getPossibleMoves(node).
            withFilter(_ => beta > alpha). // Pruning
            foreach { move =>
              val simulate = player.simulateMove(node, move)
              //printH(player, depth, simulate)
              val max1 =
                max((alpha, moveChoice), 
                  alphaBeta(simulate, 
                  depth-1, 
                  alpha, 
                  beta, 
                  move :: moveChoice, 
                  not(player), 
                  turn+1))
              alpha = max1._1
              moveChoice = max1._2
            }
            (alpha, moveChoice)
          }

          // MIN PLAYER
          case _: Player1 => {
            player.getPossibleMoves(node).
            withFilter(_ => beta > alpha). // Pruning
            foreach { move =>
              val simulate = player.simulateMove(node, move)
              //printH(player, depth, simulate)
              val min1 = 
                min((beta, moveChoice), 
                alphaBeta(simulate, 
                depth-1, 
                alpha, 
                beta, 
                moveChoice, 
                not(player), 
                turn+1))
              beta = min1._1
            }
            (beta, moveChoice)
          }
        }
      }
    }
    val (v, r) = alphaBeta(board, 5, Integer.MIN_VALUE, Integer.MAX_VALUE, List[Move](), player, turn)
    //printR(v, r)
    if (!r.isEmpty) r.head
    else player.getPossibleMoves(board).head
  }

}
