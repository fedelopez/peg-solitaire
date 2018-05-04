package cat.pseudocodi

import javafx.scene.text.TextBoundsType

import cat.pseudocodi.BoardGraph._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Cursor._
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
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
  val restart: Button = new Button {
    text = "Restart"
    onMouseClicked = (_) => resetBoard()
    prefWidth = 80
  }
  val gridId: TextField = new TextField {
    onAction = (_) => println("todo")
  }
  val buttonPane: HBox = new HBox {
    padding = Insets(20, 0, 0, 0)
    spacing = 10
    alignment = Pos.CenterRight
    children = Seq(gridId, restart)
  }
  val mainPaine: BorderPane = new BorderPane {
    padding = Insets(10)
    top = scoreValue
    center = grid()
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
  lazy val graph = BoardGraph()
  lazy val pegs: Seq[Peg] = graph.nodes.map((node: Node) => Peg(node))

  case class Peg(node: Node) extends Circle {
    val empty: BooleanProperty = BooleanProperty(node.x == 3 && node.y == 3)
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
  }

  def pegClicked(nodeClicked: Peg): Unit = {
    if (selectedNode().isDefined) {
      val origin = selectedNode().get
      if (nodeClicked.empty()) {
        val exists: Node => Boolean = node => pegs.exists(peg => peg.node == node && peg.empty())
        val originNeighbors = adjacent(graph.nodes, origin.node).filterNot(exists)
        val targetNeighbors = adjacent(graph.nodes, nodeClicked.node).filterNot(exists)
        val intersect = originNeighbors.intersect(targetNeighbors).headOption
        if (intersect.isDefined) {
          origin.empty() = true
          nodeClicked.empty() = false
          intersect.flatMap(node => pegs.find(peg => peg.node == node)).foreach(peg => peg.empty() = true)
          score() = s"Score: ${score().substring(score().lastIndexOf(" ") + 1, score().length).toInt + 1}"
        }
      }
      origin.selected() = false
      selectedNode() = Option.empty
    } else {
      nodeClicked.selected() = true
      selectedNode() = Option(nodeClicked)
    }
  }

  def resetBoard(): Unit = {
    selectedNode() = Option.empty
    score() = "Score: 0"
    pegs.foreach(p => {
      if (p.node.x == 3 && p.node.y == 3) {
        p.empty() = true
        p.selected() = false
      } else {
        p.empty() = false
        p.selected() = false
      }
    })
  }

  def grid(): GridPane = {
    val grid = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(10, 0, 0, 0)
    }
    pegs.foreach(peg => {
      val text = new Text(s"${peg.node.x},${peg.node.y}")
      text.setBoundsType(TextBoundsType.VISUAL)
      val stack = new StackPane()
      stack.children = Seq(peg, text)
      grid.add(stack, peg.node.x, peg.node.y)
    })
    grid
  }
}

