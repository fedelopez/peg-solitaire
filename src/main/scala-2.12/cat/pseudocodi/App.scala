package cat.pseudocodi

import javafx.scene.text.TextBoundsType

import cat.pseudocodi.BoardGrid.{Board, Excluded}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Cursor._
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, StrokeType}
import scalafx.scene.text.Text

object App extends JFXApp with PegSolitaire {
  val scoreValue: Text = new Text {
    text <== score
    style = "-fx-font-size: 24pt"
    fill = FloralWhite
  }
  val reset: Button = new Button {
    text = "Reset"
    onMouseClicked = (_) => resetBoard()
    prefWidth = 80
  }
  val solve: Button = new Button {
    text = "Solve!"
    onMouseClicked = (_) => solveBoard()
    prefWidth = 80
  }
  val buttonPane: HBox = new HBox {
    padding = Insets(20, 0, 0, 0)
    spacing = 10
    alignment = Pos.CenterRight
    children = Seq(solve, reset)
  }
  val mainPaine: BorderPane = new BorderPane {
    padding = Insets(10)
    top = scoreValue
    center = gridPane()
    bottom = buttonPane
  }
  stage = new JFXApp.PrimaryStage {
    title.value = "Peg Solitaire"
    scene = new Scene {
      fill = SaddleBrown
      content = mainPaine
    }
  }
}

trait PegSolitaire {

  val selectedNode: ObjectProperty[Option[Peg]] = ObjectProperty(Option.empty)
  val score: StringProperty = StringProperty("Score: 0")
  lazy val graph: Board = BoardGrid.initialise()
  lazy val pegs: Seq[Peg] = for (x <- graph.indices; y <- graph.indices if graph(x)(y) != Excluded) yield Peg(x, y)

  case class Peg(x: Int, y: Int) extends Circle {
    val empty: BooleanProperty = BooleanProperty(x == 3 && y == 3)
    val selected: BooleanProperty = BooleanProperty(false)
    centerX = 25
    centerY = 40
    radius = 20
    fill <== when(hover) choose (if (empty()) Grey else WhiteSmoke) otherwise (if (empty()) Grey else DarkOrange)
    fill <== when(empty) choose Grey otherwise DarkOrange
    cursor <== when(hover) choose Hand otherwise Default
    onMouseClicked = (_: MouseEvent) => pegClicked(this)
    stroke <== when(selected) choose LightGoldrenrodYellow otherwise Color.Transparent
    strokeType = StrokeType.Centered
    strokeWidth = 2

    def isNeighbor(other: Peg): Boolean = {
      other.x == x + 1 && other.y == y ||
        other.x == x - 1 && other.y == y ||
        other.x == x && other.y == y + 1 ||
        other.x == x && other.y == y - 1
    }
  }

  def pegClicked(clicked: Peg): Unit = {
    if (selectedNode().isDefined) {
      val origin: Peg = selectedNode().get
      if (clicked.empty()) {
        val exists: Peg => Boolean = node => pegs.exists(peg => peg == node && peg.empty())
        val originNeighbors = neighbors(origin, pegs).filterNot(exists)
        val targetNeighbors = neighbors(clicked, pegs).filterNot(exists)
        val intersect = originNeighbors.intersect(targetNeighbors).headOption
        if (intersect.isDefined) {
          origin.empty() = true
          clicked.empty() = false
          intersect.flatMap(node => pegs.find(peg => peg == node)).foreach(peg => peg.empty() = true)
          score() = s"Score: ${score().substring(score().lastIndexOf(" ") + 1, score().length).toInt + 1}"
        }
      }
      origin.selected() = false
      selectedNode() = Option.empty
    } else {
      clicked.selected() = true
      selectedNode() = Option(clicked)
    }

    def neighbors(peg: Peg, pegs: Seq[Peg]): Seq[Peg] = pegs.filter(p => peg.isNeighbor(p))
  }

  def resetBoard(): Unit = {
    selectedNode() = Option.empty
    score() = "Score: 0"
    pegs.foreach(p => {
      if (p.x == 3 && p.y == 3) {
        p.empty() = true
        p.selected() = false
      } else {
        p.empty() = false
        p.selected() = false
      }
    })
  }

  def solveBoard(): Unit = {

  }

  def gridPane(): GridPane = {
    val grid = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(10, 0, 0, 0)
    }
    pegs.foreach(peg => {
      val text = new Text(s"${peg.x},${peg.y}")
      text.setBoundsType(TextBoundsType.VISUAL)
      val stack = new StackPane()
      stack.children = Seq(peg, text)
      grid.add(stack, peg.x, peg.y)
    })
    grid
  }
}

