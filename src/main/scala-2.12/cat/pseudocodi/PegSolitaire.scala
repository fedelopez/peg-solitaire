package cat.pseudocodi

import cat.pseudocodi.Graph._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property._
import scalafx.geometry.Insets
import scalafx.scene.Cursor._
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, StrokeType}
import scalafx.scene.text.Text

object App extends JFXApp with PegSolitaire {
  val restart = new Button {
    text = "Restart"
    onMouseClicked = (_: MouseEvent) => resetGraph()
  }
  val scoreValue = new Text {
    text <== score
    style = "-fx-font-size: 24pt"
    fill = FloralWhite
  }
  val leftPane = new VBox {
    children = Seq(scoreValue, restart)
  }
  val mainPaine = new HBox {
    padding = Insets(10)
    children = Seq(leftPane, grid())
  }
  stage = new JFXApp.PrimaryStage {
    title.value = "Peg Solitaire"
    width = 600
    height = 450
    scene = new Scene {
      fill = SaddleBrown
      content = mainPaine
    }
  }
}

trait PegSolitaire {

  val selectedNode: ObjectProperty[Option[Node]] = ObjectProperty(Option.empty)

  val score: StringProperty = StringProperty("0")

  lazy val graph = Graph()

  class Peg(node: Node) extends Circle {
    centerX = 25
    centerY = 40
    radius = 20
    fill <== when(hover) choose (if (node.empty()) Grey else WhiteSmoke) otherwise (if (node.empty()) Grey else DarkOrange)
    fill <== when(node.empty) choose Grey otherwise DarkOrange
    cursor <== when(hover) choose Hand otherwise Default
    onMouseClicked = (_: MouseEvent) => pegClicked(node)
    stroke <== when(node.selected) choose LightGoldrenrodYellow otherwise Color.Transparent
    strokeType = StrokeType.Centered
    strokeWidth = 2
  }

  def pegClicked(target: Node): Unit = {
    if (selectedNode().isDefined) {
      val origin = selectedNode().get
      if (target.empty()) {
        val originNeighbors = adjacentNodes(graph.nodes, origin).filterNot(node => node.empty())
        val targetNeighbors = adjacentNodes(graph.nodes, target).filterNot(node => node.empty())
        val intersect = originNeighbors.intersect(targetNeighbors).headOption
        if (intersect.isDefined) {
          origin.empty() = true
          target.empty() = false
          intersect.foreach(node => node.empty() = true)
          score() = s"${score().toInt + 1}"
        }
      }
      origin.selected() = false
      selectedNode() = Option.empty
    } else {
      target.selected() = true
      selectedNode() = Option(target)
    }
  }

  def resetGraph(): Unit = {
    selectedNode() = Option.empty
    graph.nodes.foreach {
      case node@Node(3, 3, _, _) =>
        node.empty() = true
        node.selected() = false
      case node =>
        node.empty() = false
        node.selected() = false
    }
  }

  def grid(): GridPane = {
    val grid = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(10)
    }
    graph.nodes.foreach(n => grid.add(new Peg(n), n.x, n.y))
    grid
  }
}

