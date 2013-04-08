trait MaxMin
object Max extends MaxMin
object Min extends MaxMin

object AlphaBeta {

  type Move = IndexedSeq[State]
  type FitnessMove = Tuple2[Int, Option[Move]]

  // Debugging methods ---------------

  private def getP(p: Player) = p match {
    case _: Player1 => "Player1"
    case _: Player2 => "Player2"
  }

  private def printH(p: Player, d: Int, b: Board, turn: Int) {
    println("Player: "+ getP(p) +", Depth: "+ d +",  Heur: "+ p.evalHeuristic(b, turn))
    println(b)
  }

  private def printR(v: Int, r: Option[Move]) {
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

  def max(x: FitnessMove, y: FitnessMove) = if (x._1 >= y._1) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x._1 <= y._1) x else y
  def terminal(turn: Int) = if (turn >= 64) true else false

  def search(board: Board, player: Player, turn: Int, MAX_DEPTH: Int = 7): Move = {
    def alphaBeta(node: Board, depth: Int, alpha: Int, beta: Int, moveChoice: Option[Move], player: Player,
      p: MaxMin, turn: Int): FitnessMove =
      if (depth == 0 || terminal(turn))
        (player.evalHeuristic(node, turn), moveChoice)
      else
        p match {
          // MAX PLAYER
          case _: Max.type =>
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulate = player.simulateMove(node, move)
              max((alpha, moveChoice),
                alphaBeta(simulate, depth-1, alpha, beta, Option(move), not(player), Min, turn+1))
            }

          // MIN PLAYER
          case _: Min.type =>
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foldLeft((beta, moveChoice)) { case ((beta, _), move) =>
              val simulate = player.simulateMove(node, move)
              min((beta, moveChoice),
                alphaBeta(simulate, depth-1, alpha, beta, moveChoice, not(player), Max, turn+1))
            }
        }
    val (_, moveChoice) = alphaBeta(board, MAX_DEPTH, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max, turn)
    moveChoice getOrElse player.getPossibleMoves(board).head
  }

}
