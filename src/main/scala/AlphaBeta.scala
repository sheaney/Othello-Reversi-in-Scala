trait MaxMin
case class Max() extends MaxMin
case class Min() extends MaxMin 

object AlphaBeta {

  type Move = List[State]

  case class FitnessMove(fitness: Int, move: Move)

  def not(p: MaxMin) = p match {
    case _: Max => Min()
    case _: Min => Max()
  } 

  def not(p: Player) = p match {
    case _: Player2 => Player1()
    case _: Player1 => Player2()
  }

  def max(x: FitnessMove, y: FitnessMove) = if (x.fitness >= y.fitness) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x.fitness <= y.fitness) x else y
  def terminal(turn: Int) = if (turn >= 64) true else false

  def search(board: Board, player: Player, turn: Int): Move = {
    def alphaBeta(node: Board, depth: Int, a: Int, b: Int, r: Move, player: Player,
      p: MaxMin, turn: Int): FitnessMove = {
      var alpha = a
      var beta = b
      var moveChoice = r
      if (depth == 0 || terminal(turn)) {
        FitnessMove(player.evalHeuristic(node), r)
      }
      else {
        p match {
          // MAX PLAYER
          case _: Max => {
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foreach { move =>
              val simulate = player.simulateMove(node, move)
              val max1 =
                max(FitnessMove(alpha, moveChoice), 
                  alphaBeta(simulate, depth-1, alpha, beta, move, not(player), not(p), turn+1))
              alpha = max1.fitness
              moveChoice = max1.move
            }
            FitnessMove(alpha, moveChoice)
          }

          // MIN PLAYER
          case _: Min => {
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foreach { move =>
              val simulate = player.simulateMove(node, move)
              val min1 = 
                min(FitnessMove(beta, moveChoice),
                  alphaBeta(simulate, depth-1, alpha, beta, moveChoice, not(player), not(p), turn+1))
              beta = min1.fitness
            }
            FitnessMove(beta, moveChoice)
          }
        }
      }
    }
    val fitnessMove = alphaBeta(board, 5, Integer.MIN_VALUE, Integer.MAX_VALUE, List[State](), player, Max(), turn)
    if (!fitnessMove.move.isEmpty) fitnessMove.move
    else player.getPossibleMoves(board).head
  }

}

