package fhj.swengb.assignments.tree

/**
  * a package object is a place to store functions and other stuff which
  * should be visible only in this package.
  */
package object aseebacher {

  type AngleInDegrees = Double
  type AngleInRadians = Double

  sealed trait Tree[+T]

  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  case class Node[T](value: T) extends Tree[T]


}
