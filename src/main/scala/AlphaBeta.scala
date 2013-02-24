
trait MaxMin
case class Max() extends MaxMin
case class Min() extends MaxMin 

object AlphaBeta {

  type Move = List[State]

  case class FitnessMove(fitness: Int, move: List[Move])

  def not(p: Player) = p match {
    case _: Player2 => Player1()
    case _: Player1 => Player2()
  }

  def max(x: FitnessMove, y: FitnessMove) = if (x.fitness >= y.fitness) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x.fitness <= y.fitness) x else y
  def terminal(turn: Int) = if (turn >= 64) true else false 

  def search(board: Board, player: Player, turn: Int): Move = {
    def alphaBeta(node: Board, depth: Int, alpha: Int, beta: Int, moveChoice: List[Move], player: Player, 
      p: MaxMin, turn: Int): FitnessMove = {

      if (depth == 0 || terminal(turn)) FitnessMove(player.evalHeuristic(node), moveChoice)
      else
        p match {
          // MAX PLAYER
          case _: Max =>
            val fitnessMove = FitnessMove(alpha, moveChoice)
            player.getPossibleMoves(node).
            filter(_ => beta > alpha). // Pruning
            foldLeft(fitnessMove) { (fitMove, move) =>
              val simulate = player.simulateMove(node, move)
              max(FitnessMove(alpha, moveChoice),
                alphaBeta(simulate, depth-1, alpha, beta, move :: moveChoice, not(player), Min(), turn+1))
            }

          // MIN PLAYER
          case _: Min =>
            val fitnessMove = FitnessMove(beta, moveChoice)
            player.getPossibleMoves(node).
            filter(_ => beta > alpha). // Pruning
            foldLeft(fitnessMove) { (fitMove, move) =>
              val simulate = player.simulateMove(node, move)
              val fm = 
                min(FitnessMove(beta, moveChoice),
                  alphaBeta(simulate, depth-1, alpha, beta, moveChoice, not(player), Max(), turn+1))
              FitnessMove(fm.fitness, moveChoice)
            }
        }
    }

    val fitnessMove: FitnessMove =
      alphaBeta(board, 5, Integer.MIN_VALUE, Integer.MAX_VALUE, List[Move](), player, Max(), turn)
    if (!fitnessMove.move.isEmpty) fitnessMove.move.head
    else player.getPossibleMoves(board).head
  }

}
