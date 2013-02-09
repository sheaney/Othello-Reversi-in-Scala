import scala.swing._

import java.awt.{Dimension, Color}
import javax.swing.{ImageIcon, SwingUtilities}

class GUI(private val gameBoard: Board) {
  import GUI.{selection, pingPong}

  private var currentTurn: Int = 1
  private val whiteDisk = new Label { icon = new ImageIcon("images/white.gif") }
  private val blackDisk = new Label { icon = new ImageIcon("images/black.gif") }
  private val empty = new Label("")

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
      renderCell(row, column)
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

  private def renderCell(row: Int, column: Int): Component = gameBoard.getDisk(row, column) match {
    case 1 => blackDisk
    case 2 => whiteDisk
    case _ => empty
  }

  def startGUI() {
    SwingUtilities.invokeLater(new Runnable {
      def run {
        mainFrame.visible = true  
      }
    })
  }

  def tryMove(row: Int, column: Int) {
    selection = (row -> column)
  }

  def cannotMove(turnNo: Int): Boolean = {
    currentTurn match {
      case 1 =>
        message.text = "White has nowhere to move"
      case 2 =>
        message.text = "Black has nowhere to move"
    }
    // Let user digest the message
    Thread.sleep(2500)

    if (pingPong == turnNo)
      true
    else {
      pingPong = turnNo
      false
    }
  }

  def winner(p: AnyRef) {
    p match {
      case _: Human =>
        message.text = "White wins!"
      case _: Computer =>
        message.text = "Black wins!"
      case _ => "It's a tie!"
    }
  }

  def update(turn: Int) {
    val acc = gameBoard.countDisks
    table.repaint
    diskCount.text = "White: "+ acc.p1Disks +"   Black: "+ acc.p2Disks
    turn match {
      case 1 =>
        message.text = "Black's move"
      case 2 =>
        message.text = "White's move"
    }

    currentTurn = turn % 2 + 1
  }

}

object GUI {
  private var selection = (-1 -> -1)
  private var pingPong = 0

  def awaitMoveSelection: (Int, Int) = {
    refreshSelection()
    while (!hasChosenMove) {
      Thread.sleep(500)
    }

    selection
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
