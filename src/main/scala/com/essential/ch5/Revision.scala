package com.essential.ch5

sealed trait IntList {
  def length: Int =
    fold[Int](0, (x, y) => 1 + y)

  def double: IntList =
    fold[IntList](End, (hd, tl) => Pair(hd * 2, tl))

  def product: Int =
    fold[Int](1, (x, y) => x * y)

  def sum: Int =
    fold[Int](0, (x, y) => x + y)

  def fold[A](end: A, f: (Int, A) => A): A =
    this match {
      case End => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
}

final case object End extends IntList

final case class Pair(head: Int, tail: IntList) extends IntList

object Temp {
  def main(args: Array[String]) {
    val example = Pair(1, Pair(2, Pair(3, End)))
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End.length == 0)

  }
}

/*sealed trait LinkedList[A] {
  def length: Int =
    this match {
      case Pair(hd, tl) => 1 + tl.length
      case End() => 0
    }
  def contains(elem:A):Boolean =
    this match {
      case Pair(hd, _) if hd == elem => true
      case Pair(hd, tl) if hd != elem => tl.contains(elem)
      case End() => false
    }
  def apply(index:Int):Result[A] =
    this match {
      case Pair(hd, _) if index == 0 => Success(hd)
      case Pair(_, tl) if index != 0 => tl.apply(index-1)
      case End() => Failure("Index out of bounds")
    }
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

object Temp {
  def main(args:Array[String]) {
    val example = Pair(1, Pair(2, Pair(3, End())))
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End().length == 0)

    assert(example.contains(3) == true)
    assert(example.contains(4) == false)
    assert(End().contains(0) == false)

    assert(example(0) == Success(1))
    assert(example(1) == Success(2))
    assert(example(2) == Success(3))
    assert(example(3) == Failure("Index out of bounds"))
  }
}
*/
