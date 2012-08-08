import scala.swing._
import scala.actors._
import scala.actors.Actor._

import java.awt.{Dimension, Color}
import javax.swing.{ImageIcon, SwingUtilities}

class GUI(private var gameBoard: Board) {
  import GUI.selection

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

  private val mainFrame = new MainFrame {
    title = "Reversi"
    contents = table
  }

  private def renderCell(row: Int, column: Int): Component = gameBoard.board(row)(column) match {
    case 1 =>
      new Label {
        icon = new ImageIcon("black.gif")
      }
    case 2 => 
      new Label {
        icon = new ImageIcon("white.gif")
      }
    case _ => new Label("")
  }

  val worker = actor {

    SwingUtilities.invokeLater(new Runnable() {
      def run {
        mainFrame.visible = true
      }
    })
    
    loop {
      react {
        case Board(game) =>
          mainFrame.repaint
      }
    }
  }

  def tryMove(row: Int, column: Int) {
    Game.currentTurn match {
      case p: Human =>
        selection = (row -> column)
      case _ =>
    }
  }

  def update(board: Board) {
    this.worker ! board
  }
}

object GUI {
  private var selection = (-1 -> -1)

  def awaitMoveSelection: (Int,Int) = {
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
