package com.essential.ch4.ch4_6

sealed trait IntList {
  def length: Int = this match {
    case End => 0
    case Pair(_, tl) => 1 + tl.length
  }

  def product: Int = this match {
    case End => 1
    case Pair(hd, tl) => hd * tl.product
  }

  def double: IntList = this match {
    case End => End
    case Pair(hd, tl) => Pair(hd * 2, tl.double)
  }
}

final case object End extends IntList

final case class Pair(head: Int, tail: IntList) extends IntList

sealed trait Tree {
  def sum: Int

  def product: Tree
}

final case class Node(left: Tree, right: Tree) extends Tree {
  val sum = left.sum + right.sum
  val product = Node(left.product, right.product)
}

final case class Leaf(value: Int) extends Tree {
  val sum = value
  val product = Leaf(2 * value)
}


object Main {
  val example = Pair(1, Pair(2, Pair(3, End)))

  def main(args: Array[String]): Unit = {
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End.length == 0)

    assert(example.product == 6)
    assert(example.tail.product == 6)
    assert(End.product == 1)

    assert(example.double == Pair(2, Pair(4, Pair(6, End))))
    assert(example.tail.double == Pair(4, Pair(6, End)))
    assert(End.double == End)

  }
}
