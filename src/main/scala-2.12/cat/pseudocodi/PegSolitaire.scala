package cat.pseudocodi

import scala.collection.immutable.IndexedSeq
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Cursor._
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text

object App extends JFXApp with PegSolitaire {
  val clear = new Button {
    text = "Clear"
  }
  val restart = new Button {
    text = "Restart"
  }
  val scoreText = new Text {
    text = "Score: 0"
    style = "-fx-font-size: 24pt"
    fill = FloralWhite
  }
  val buttonPane = new HBox {
    padding = Insets(10, 0, 0, 0)
    spacing = 10
    children = Seq(clear, restart)
  }
  val leftPane = new VBox {
    children = Seq(scoreText, buttonPane)
  }
  val mainPaine = new HBox {
    padding = Insets(10)
    children = Seq(leftPane, GridFactory.grid())
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

  class Peg(node: Node) extends Circle {
    centerX = 25
    centerY = 40
    radius = 20
    fill <== when(hover) choose (if (node.empty) Grey else FloralWhite) otherwise (if (node.empty) Grey else DarkOrange)
    cursor <== when(hover) choose (if (node.empty) Default else Hand) otherwise Default
    onMouseClicked = (_: MouseEvent) => println("Clicked!!")
  }

  object GridFactory {
    def grid(): GridPane = {
      val grid = new GridPane {
        hgap = 10
        vgap = 10
        padding = Insets(10)
      }
      GraphFactory.graph().nodes.foreach(n => grid.add(new Peg(n), n.y, n.x))
      grid
    }
  }

  object GraphFactory {
    def adjacentNodes(nodes: IndexedSeq[Node], n: Node): Seq[Node] = {
      nodes.filter(node => n.isNeighbor(node))
    }

    def removeDups(edges: Seq[Edge]): Seq[Edge] = {
      def doRemoveDups(acc: Seq[Edge], res: Seq[Edge]): Seq[Edge] = {
        if (acc.isEmpty) res
        else {
          if (res.exists(edge => acc.head.a == edge.a && acc.head.b == edge.b || acc.head.a == edge.b && acc.head.b == edge.a)) {
            doRemoveDups(acc.tail, res)
          } else {
            doRemoveDups(acc.tail, res :+ acc.head)
          }
        }
      }

      doRemoveDups(edges, List())
    }

    def graph(): Graph = {
      val excluded = List(Rect(0, 0, 1), Rect(0, 5, 1), Rect(5, 0, 1), Rect(5, 5, 1))
      val nodes = for (x <- 0 until 7; y <- 0 until 7 if !excluded.exists(r => r.intersects(x, y)))
        yield Node(s"($x,$y)", x, y, empty = x == 3 && y == 3)
      val edges = nodes.flatMap(n => adjacentNodes(nodes, n).map(neighbor => Edge(n, neighbor)))
      Graph(nodes, removeDups(edges))
    }
  }

  case class Rect(x: Int, y: Int, len: Int) {
    def intersects(x: Int, y: Int): Boolean =
      (x >= this.x && x <= this.x + len) && (y >= this.y && y <= this.y + len)
  }

  case class Graph(nodes: Seq[Node], edges: Seq[Edge])

  case class Node(name: String, x: Int, y: Int, empty: Boolean = false) {
    def isNeighbor(other: Node): Boolean = {
      other.x == x + 1 && other.y == y ||
        other.x == x - 1 && other.y == y ||
        other.x == x && other.y == y + 1 ||
        other.x == x && other.y == y - 1
    }
  }

  case class Edge(a: Node, b: Node)

}

