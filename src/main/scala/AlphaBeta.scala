trait MaxMin
case class Max() extends MaxMin
case class Min() extends MaxMin

class FitnessMove(val fitness: Int, val move: List[State])

object AlphaBeta {

  type Move = List[State]

  private def not(p: Player) = p match {
    case _: Player2 => Player1()
    case _: Player1 => Player2()
  }

  private def availableMove(fitnessMove: FitnessMove) =
    if (fitnessMove.move.isEmpty) false else true

  private def max(x: FitnessMove, y: FitnessMove) = if (x.fitness >= y.fitness) x else y
  private def min(x: FitnessMove, y: FitnessMove) = if (x.fitness <= y.fitness) x else y
  private def terminal(turn: Int) = if (turn >= 64) true else false

  def search(board: Board, player: Player, turn: Int): Move = {
    def alphaBeta(node: Board, depth: Int, alpha: Int, beta: Int, moveChoice: Move, player: Player,
      p: MaxMin, turn: Int): FitnessMove = {
      if (depth == 0 || terminal(turn))
        new FitnessMove(player.evalHeuristic(node, turn), moveChoice)
      else
        p match {
          case _: Max => {
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foldLeft(new FitnessMove(alpha, moveChoice)) { (fitnessMove, move) =>
              val simulate = player.simulateMove(node, move)
              max(fitnessMove, alphaBeta(simulate, depth-1, alpha, beta, move, not(player), Min(), turn+1))
            }
          }
          case _: Min => {
            player.getPossibleMoves(node).
            takeWhile(_ => beta > alpha). // Pruning
            foldLeft(new FitnessMove(alpha, moveChoice)) { (fitnessMove, move) =>
              val simulate = player.simulateMove(node, move)
              min(fitnessMove, alphaBeta(simulate, depth-1, alpha, beta, moveChoice, not(player), Max(), turn+1))
            }
          }
        }
    }

    val fitnessMove = alphaBeta(board, 3, Integer.MIN_VALUE, Integer.MAX_VALUE, List[State](), player, Max(), turn)
    if (availableMove(fitnessMove)) fitnessMove.move
    else player.getPossibleMoves(board).head
  }

}

