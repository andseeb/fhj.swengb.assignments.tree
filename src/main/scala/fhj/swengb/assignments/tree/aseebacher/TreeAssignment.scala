package fhj.swengb.assignments.tree.aseebacher

import javafx.scene.paint.Color

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object Graph {

  val colorMap =
    Map[Int, Color](
      0 -> Color.ROSYBROWN,
      1 -> Color.BROWN,
      2 -> Color.SADDLEBROWN,
      3 -> Color.INDIANRED,
      4 -> Color.DARKGREEN,
      5 -> Color.GREEN,
      6 -> Color.YELLOWGREEN,
      7 -> Color.GREENYELLOW,
      8 -> Color.YELLOW
    )

  /**
    * creates a random tree
    *
    * @param pt
    * @return
    */
  def randomTree(pt: Pt2D): Tree[L2D] =
    mkGraph(pt, Random.nextInt(360), Random.nextDouble() * 150, Random.nextInt(7))


  /**
    * Given a Tree of L2D's and a function which can convert any L2D to a Line,
    * you have to traverse the tree (visit all nodes) and create a sequence
    * of Line's. The ordering of the lines is not important.
    *
    * @param tree  a tree which contains L2D instances
    * @param convert a converter function
    * @return
    */
  def traverse[A, B](tree: Tree[A])(convert: A => B): Seq[B] = {

    def recTraverse[A, B](tree: Tree[A], list: Seq[B])(convert: A => B): Seq[B] = {
      tree match {
        case Branch(left, right) => {
          list ++ recTraverse(left, list)(convert) ++ recTraverse(right, list)(convert) // append all 3 lists to each other
        }
        case Node(v) => {
          list :+ convert(v) // append value to list
        }
      }
    }


    recTraverse(tree, Nil)(convert)

  }

  /**
    * Creates a tree graph.
    *
    * @param start the startpoint (root) of the tree
    * @param initialAngle initial angle of the tree
    * @param length the initial length of the tree
    * @param treeDepth the depth of the tree
    * @param factor the factor which the length is decreasing for every iteration
    * @param angle the angle between a branch and the root
    * @param colorMap color map, by default it is the colormap given in the companion object Graph
    *
    * @return a Tree[L2D] which can be traversed by other algorithms
    */
  def mkGraph(start: Pt2D,
              initialAngle: AngleInDegrees,
              length: Double,
              treeDepth: Int,
              factor: Double = 0.75,
              angle: Double = 45.0,
              colorMap: Map[Int, Color] = Graph.colorMap): Tree[L2D] = {
    assert(treeDepth <= colorMap.size, s"Treedepth higher than color mappings - bailing out ...")
    // L2D hat start,end,color
    // L2D apply macht einen L2D aus start,angle,length,color


    def recMkGraph(curDepth: Int,
                   curLink: L2D,
                   curStart: Pt2D,
                   deltaAngle : Double) : Tree[L2D] = {
      if (treeDepth == 0) { // special case
        Node(curLink)
      } else if (curDepth +1 == treeDepth) { // at the end


        Branch(
          Node(curLink),
          Branch(
            Node(curLink.left(factor, angle, colorMap(curDepth))),
            Node(curLink.right(factor, angle, colorMap(curDepth))))
        )


      } else {


        Branch(
          Node(curLink),
          Branch(
            recMkGraph(curDepth+1, curLink.left(factor,angle,colorMap(curDepth)), curLink.end, angle),
            recMkGraph(curDepth+1, curLink.right(factor,angle,colorMap(curDepth)), curLink.end, angle)
          )
        )


      }
    }


    recMkGraph(0, L2D(start, initialAngle, length, colorMap(0)), start, 0)

  }

}

object MathUtil {

  /**
    * rounds the given value to 3 decimal places.
    *
    * @param value  a double value
    * @return
    */
  def round(value: Double): Double = {
    // http://stackoverflow.com/a/11107005
    BigDecimal(value).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


  /**
    * normalises a given degree angle to a number from 0 to 360
    *
    * @param angle  a double value
    * @return
    */
  def normaliseDegreeAngle(angle: Double) : Double = {

    def increaseAngle(angle: Double): Double = {
      if (angle < 0) increaseAngle(angle+360)
      else angle
    }

    def decreaseAngle(angle: Double): Double = {
      if (angle > 360) decreaseAngle(angle-360)
      else angle
    }

    if (angle < 0) increaseAngle(angle)
    else decreaseAngle(angle)


  }

}


object L2D {

  import MathUtil._

  /**
    * Given a startpoint, an angle and a length the endpoint of the line
    * is calculated and finally a L2D class is returned.
    *
    * @param start the startpoint
    * @param angle the angle
    * @param length the length of the line
    * @param color the color
    * @return
    */
  def apply(start: Pt2D, angle: AngleInDegrees, length: Double, color: Color): L2D = {
    // get angle that is between 0 and 360



    // add the length to the corresponding directions
    /*val endPoint = normaliseDegreeAngle(angle) match {
      case alpha if (alpha >= 0 && alpha <= 90) => {
        //sin(angle.toRadians) = opposedCathetus / hypothenuse
        //sin(angle.toRadians) * hypothenuse = opposedCathetus
        //opposedCathetus = sin(angle.toRadians) * hypothenuse
        val opposedCathetusLength = Math.sin(angle.toRadians) * length

        //cos(angle.toRadians) = adjacentCathetus / hypothenuse
        val adjacentCathetusLength = Math.cos(angle.toRadians) * length

        (MathUtil.round(start.x + adjacentCathetusLength), MathUtil.round(start.y + opposedCathetusLength))
      }
      case alpha if (alpha > 90 && alpha <= 180) => {
        val opposedCathetusLength = Math.sin((angle-90).toRadians) * length
        val adjacentCathetusLength = Math.cos((angle-90).toRadians) * length
        (MathUtil.round(start.x - opposedCathetusLength), MathUtil.round(start.y + adjacentCathetusLength))
      }
      case alpha if (alpha > 180 && alpha <= 270) => {
        val opposedCathetusLength = Math.sin((angle-180).toRadians) * length
        val adjacentCathetusLength = Math.cos((angle-180).toRadians) * length
        (MathUtil.round(start.x - opposedCathetusLength), MathUtil.round(start.y - adjacentCathetusLength))
      }
      case alpha if (alpha > 270 && alpha <= 360) => {
        val opposedCathetusLength = Math.sin((angle-270).toRadians) * length
        val adjacentCathetusLength = Math.cos((angle-270).toRadians) * length
        (MathUtil.round(start.x + adjacentCathetusLength), MathUtil.round(start.y - opposedCathetusLength))
      }
    }*/

    val normAngle = normaliseDegreeAngle(angle)

    //sin(angle.toRadians) = opposedCathetus / hypothenuse
    //sin(angle.toRadians) * hypothenuse = opposedCathetus
    //opposedCathetus = sin(angle.toRadians) * hypothenuse
    val opposedCathetusLength = Math.sin(normAngle.toRadians) * length

    //cos(angle.toRadians) = adjacentCathetus / hypothenuse
    val adjacentCathetusLength = Math.cos(normAngle.toRadians) * length

    val endPoint = (MathUtil.round(start.x + adjacentCathetusLength), MathUtil.round(start.y + opposedCathetusLength))



    // http://michalostruszka.pl/blog/2015/03/30/scala-case-classes-to-and-from-tuples/
    L2D(start, Pt2D.tupled(endPoint), color)
  }


}

case class L2D(start: Pt2D, end: Pt2D, color: Color) {

  lazy val xDist = end.x - start.x
  lazy val yDist = end.y - start.y

  lazy val angle = {
    assert(!((xDist == 0) && (yDist == 0)))
    (xDist, yDist) match {
      case (x, 0) if x > 0 => 0
      case (0, y) if y > 0 => 90
      case (0, y) if y < 0 => 270
      case (x, 0) if x < 0 => 180
      case (x, y) if x < 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x < 0 && y > 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x > 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 360
      case (x, y) => Math.atan(y / x) * 180 / Math.PI
    }
  }

  lazy val length: Double = {
    Math.sqrt(xDist * xDist + yDist * yDist)
  }

  def left(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle - deltaAngle, length * factor, c)
  }

  def right(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle + deltaAngle, length * factor, c)
  }


}

