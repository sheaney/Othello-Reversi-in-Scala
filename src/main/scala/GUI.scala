import scala.swing._
import scala.swing.event.ButtonClicked

import java.awt.{Dimension, Color}
import javax.swing.{ImageIcon, SwingUtilities}


class GUI(private val gameBoard: Board) {
  import GUI.{selection, pingPong}
  
  private val whiteDisk = new ImageIcon("images/white.gif")
  private val blackDisk = new ImageIcon("images/black.gif")

  private val table = new Table(8, 8) {                                 
    background = new Color(0, 100, 0)
    autoResizeMode = Table.AutoResizeMode.Off
    rowHeight = 30
    preferredSize = { new Dimension(300, 240) }
    gridColor = Color.BLACK
    selection.elementMode = Table.ElementMode.Cell
                                                                 
    override def rendererComponent(isSelected: Boolean,
        hasFocus: Boolean, row: Int, column: Int): Component = {
      if (hasFocus) tryMove(row, column)
      renderCell(row, column, gameBoard) 
    }
  }

  private val message = new Label("White's move")
  private val diskCount = new Label("White: 2   Black: 2")

  private val mainFrame = new MainFrame {
    title = "Reversi"
    location = new Point(200, 200)
    resizable = false
    contents = new BoxPanel(Orientation.Vertical) {
      contents += diskCount
      contents += message
      contents += table
    }
  }

  private def renderCell(row: Int, column: Int, board: Board): Component = gameBoard.board(row)(column) match {
    case 1 =>
      new Label {
        icon = blackDisk 
      }
    case 2 => 
      new Label {
        icon = whiteDisk 
      }
    case _ => new Label("")
  }

  def startGUI() {
    SwingUtilities.invokeLater(new Runnable {
      def run {
        mainFrame.visible = true  
      }
    })
  }

  def tryMove(row: Int, column: Int) {
    Game.currentTurn match {
      case p: Human => 
        selection = (row -> column)
      case _ =>
    }
  }

  def cannotMove: Boolean = {
    Game.currentTurn match {
      case Human() =>
        message.text = "White has nowhere to move"
      case Computer() =>
        message.text = "Black has nowhere to move"
    }
    // Let user digest the message
    Thread.sleep(2500)

    if (pingPong == Game.turnNo - 1)
      true
    else {
      pingPong = Game.turnNo
      false
    }
  }

  def winner(p: Any) {
    p match {
      case Human() =>
        message.text = "You won!"
      case Computer() =>
        message.text = "Better luck next time..."
      case _ => "It's a tie!"
    }
  }

  def update() {
    val acc = gameBoard.countDisks
    table.repaint
    diskCount.text = "White: "+ acc.p1Disks +"   Black: "+ acc.p2Disks
    Game.currentTurn match {
      case Human() =>
        message.text = "Black's move"
      case Computer() =>
        message.text = "White's move"
    }
  }

}

object GUI {
  private var selection = (-1 -> -1)
  private var pingPong = 0

  def awaitMoveSelection: (Int, Int) = {
    refreshSelection()
    loopWhile(!hasChosenMove) {
      Thread.sleep(500)
    }

    selection
  }

  private def loopWhile(cond: => Boolean)(body: => Unit) {
    while (cond) body
  }

  private def hasChosenMove: Boolean = {
    val (x, y) = selection
    if (x > -1 && y > -1) true
    else false
  }

  private def refreshSelection() {
    selection = (-1 -> -1)
  }
}
