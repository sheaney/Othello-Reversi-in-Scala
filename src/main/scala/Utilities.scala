trait Utilities {

  val upperLimit, leftLimit = 0
  val lowerLimit, rightLimit = 7

  // Guards that check that the given indeces are within the board limits
  val upLeftDiagonalCheck = (i: Int, j: Int) => i >= upperLimit && j >= leftLimit 
  val upRightDiagonalCheck = (i: Int, j: Int) => i >= upperLimit && j <= rightLimit
  val downRightDiagonalCheck = (i: Int, j: Int) => i <= lowerLimit && j <= rightLimit
  val downLeftDiagonalCheck = (i: Int, j: Int) => i <= lowerLimit && j >= leftLimit
  val leftCheck = (i: Int, j: Int) => j >= leftLimit 
  val upCheck = (i: Int, j: Int) => i >= upperLimit 
  val rightCheck = (i: Int, j: Int) => j <= rightLimit 
  val downCheck = (i: Int, j: Int) => i <= lowerLimit

  // Directional movements
  val left, up = (x: Int) => x - 1
  val right, down = (x: Int) => x + 1 
  val none = (x: Int) => x

  case class Accumulator(p1Disks: Int = 0, p2Disks: Int = 0)

}
