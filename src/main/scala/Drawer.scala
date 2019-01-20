import core._
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.util.Duration

import scala.language.postfixOps

object Drawer extends JFXApp {

  var activeGrid: Grid = new ClassicGrid(10, 20, List())
  var activeBlock: Tetromino = TetrisFuncs.getNextBlock()
  var gridToDraw: Grid = activeGrid

  def getColor(c: Int): Color = {

    c match {
      case 0 => Gray
      case 1 => Green
      case 2 => Red
      case 3 => Yellow
      case 4 => Orange
      case 5 => Blue
      case 6 => Cyan
      case 7 => Purple
      case 8 => White
      case _ => Black
    }
  }

  def computeNextGrid(): Option[List[Int]] = {
    val newBlock = TetrisFuncs.block_move(activeBlock, 0, -1)
    if (activeGrid.isValidPlace(newBlock)) {
      gridToDraw = activeGrid.placeBlock(newBlock)
      activeBlock = newBlock
      None
    } else {
      activeGrid = activeGrid.placeBlock(activeBlock)
      activeBlock = TetrisFuncs.getNextBlock()
      gridToDraw = activeGrid
      Some(activeGrid.getClearedLines())
    }
  }


  val canvas: Canvas = new Canvas(gridToDraw.w * 24 + 60, gridToDraw.h * 25)
  val gc: GraphicsContext = canvas.graphicsContext2D
  val s: Scene = new Scene {
    content = canvas
    onKeyPressed = (e: KeyEvent) => {
      handleControl(e)
    }
  }


  stage = new PrimaryStage {
    title = "it's fucking tetris"
    scene = s
  }

  def updateActiveBlock(nb: Tetromino): Unit = {
    activeBlock = nb
    gridToDraw = activeGrid.placeBlock(nb)
  }

  def moveBlockToDraw(xd: Int, yd: Int): Boolean = {
    val nb = TetrisFuncs.block_move(activeBlock, xd, yd)
    if (activeGrid.isValidPlace(nb)) {
      updateActiveBlock(nb)
      return true
    }

    false
  }

  def spinBlockToDraw(dd: Int): Boolean = {
    val nb = TetrisFuncs.block_spin(activeBlock, dd)
    if (activeGrid.isValidPlace(nb)) {
      updateActiveBlock(nb)
      return true
    }

    false
  }

  def hardDrop(): Boolean = if (moveBlockToDraw(0, -1)) hardDrop() else true

  def handleControl(e: KeyEvent): Unit = {
    e.code match {
      case KeyCode.Left => moveBlockToDraw(-1, 0)
      case KeyCode.Right => moveBlockToDraw(1, 0)
      case KeyCode.Down => moveBlockToDraw(0, -1)
      case KeyCode.Up => spinBlockToDraw(1)
      case KeyCode.Space => hardDrop()
      case KeyCode.W => spinBlockToDraw(-1)
      case KeyCode.X => spinBlockToDraw(1)
      case KeyCode.T => activeGrid.getClearedLines()
      case _ =>
    }

    generate_scene(gridToDraw, gc)
  }

  val timerClear = new PauseTransition(Duration(50))
  timerClear.onFinished = _ => {
    //println("line clear")
    activeGrid = activeGrid.clearLines()
    fun()
    timer.playFromStart()

  }

  def fun(): Unit = {
    val clear = computeNextGrid()
    generate_scene(gridToDraw, gc, clear)
    if (activeGrid.getClearedLines().isEmpty)
      timer.playFromStart()
    else
      timerClear.playFromStart()
  }

  val timer = new PauseTransition(Duration(500))
  timer.onFinished = _ => fun()

  timer.play()


  def generate_scene(grid: Grid, gc: GraphicsContext, cleared : Option[List[Int]] = None): Unit = {
    val w = 0 until grid.w
    val h = 0 until grid.h
    gc.fill = getColor(-1)
    gc.fillRect(0, 0, grid.w * 24 + 60, grid.h * 25)
    val coords = w.flatMap(i => h.map(j => (i, j)))
    val cases = coords.map(t => {

      gc.fill = {
        val c = if (cleared.isDefined) cleared.get else List[Int]()
        if (c.contains(t._2))
          getColor(8)
        else {
          val b = grid.getBlockAt(t._1, t._2)
          b match {
            case Some(value) => getColor(value.color)
            case None => getColor(0)
          }
        }
      }

      gc.fillRect(
        t._1 * 25 + 1,
        (grid.h - t._2) * 25 + 1,
        24,
        24
      )
      t
    })
  }
}