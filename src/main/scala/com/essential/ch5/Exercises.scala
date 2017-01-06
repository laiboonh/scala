package com.essential.ch5

object Q5_2_3 {

  sealed trait IntList {
    def length: Int =
      fold[Int](0, (_, x2) => 1 + x2)

    def double: IntList =
      fold[IntList](End, (x1: Int, x2: IntList) => Pair(x1 * 2, x2))

    def product: Int =
      fold[Int](1, (x1, x2) => x1 * x2)

    def sum: Int =
      fold[Int](0, (x1, x2) => x1 + x2)


    def fold[A](end: A, f: (Int, A) => A): A =
      this match {
        case End => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }
  }

  final case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  def main(args: Array[String]): Unit = {
    val example = Pair(1, Pair(2, Pair(3, End)))
    assert(example.length == 3)
    assert(example.sum == 6)
    assert(example.product == 6)
    assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  }
}

object Q5_3_4 {

  trait Tree[A] {
    def fold[B](leaf: A => B, f: (B, B) => B): B =
      this match {
        case Leaf(e) => leaf(e)
        case Node(l, r) => f(l.fold(leaf, f), r.fold(leaf, f))
      }
  }

  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](elem: A) extends Tree[A]

  def main(args: Array[String]): Unit = {
    val tree: Tree[String] =
      Node(Node(Leaf("To"), Leaf("iterate")),
        Node(Node(Leaf("is"), Leaf("human,")),
          Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

    println(tree.fold[String](elem => elem, (acc1, acc2) => acc1 + " " + acc2))
  }

}

object Q5_4_1 {

  class Pair[A, B](val one: A, val two: B)

  def main(args: Array[String]): Unit = {
    val pair = new Pair[String, Int]("hi", 2)
    // pair: Pair[String,Int] = Pair(hi,2)

    println(pair.one)
    // res: String = hi

    println(pair.two)

  }

}

object Q5_4_3 {

  trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C = this match {
      case Left(v) => left(v)
      case Right(v) => right(v)
    }
  }

  final case class Left[A, B](value: A) extends Sum[A, B]

  final case class Right[A, B](value: B) extends Sum[A, B]

  def main(args: Array[String]): Unit = {
    assert(Left[Int, String](1).value == 1)
    // res: Int = 1

    assert(Right[Int, String]("foo").value == "foo")
    // res: String = foo

    val sum: Sum[Int, String] = Right("foo")
    // sum: Sum[Int,String] = Right(foo)”

    val res = sum match {
      case Left(x) => x.toString
      case Right(x) => x
    }
    assert(res == "foo")
  }

}

object Q5_4_4 {

  trait Maybe[A] {
    def fold[B](empty: B, full: A => B): B = this match {
      case Empty() => empty
      case Full(v) => full(v)
    }

    def map[B](f: A => B): Maybe[B] =
      flatMap(v => Full(f(v)))

    def flatMap[B](f: A => Maybe[B]): Maybe[B] =
      this match {
        case Empty() => Empty[B]()
        case Full(v) => f(v)
      }
  }

  final case class Empty[A]() extends Maybe[A]

  final case class Full[A](value: A) extends Maybe[A]

  val perhaps1: Maybe[Int] = Empty[Int]()
  // perhaps: Maybe[Int] = Empty()

  val perhaps2: Maybe[Int] = Full(1)
  // perhaps: Maybe[Int] = Full(1)

}

object Q5_5_4 {

  sealed trait LinkedList[A] {
    def length: Int = this match {
      case End() => 0
      case Pair(_, tl) => 1 + tl.length
    }

    def contains(elem: A): Boolean = this match {
      case End() => false
      case Pair(hd, _) if hd == elem => true
      case Pair(_, tl) => tl.contains(elem)
    }

    def apply(index: Int): Result[A] =
      this match {
        case End() => RFailure("Index out of bounds")
        case Pair(hd, _) if index == 0 => RSuccess(hd)
        case Pair(_, tl) => tl.apply(index - 1)
      }

    def fold[B](end: B, f: (A, B) => B): B =
      this match {
        case End() => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }

    def map[B](f: A => B): LinkedList[B] =
      this match {
        case End() => End[B]()
        case Pair(hd, tl) => Pair(f(hd), tl.map(f))
      }
  }

  final case class End[T]() extends LinkedList[T]

  final case class Pair[T](head: T, tail: LinkedList[T]) extends LinkedList[T]

  sealed trait Result[A]

  case class RSuccess[A](result: A) extends Result[A]

  case class RFailure[A](reason: String) extends Result[A]

  def main(args: Array[String]): Unit = {
    val example = Pair(1, Pair(2, Pair(3, End())))
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End().length == 0)

    assert(example.contains(3))
    assert(!example.contains(4))
    assert(!End().contains(0))
    // This should not compile
    // example.contains("not an Int")

    assert(example(0) == Success(1))
    assert(example(1) == Success(2))
    assert(example(2) == Success(3))
    assert(example(3) == Failure("Index out of bounds"))

    val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))
    assert(list.map(_ * 2) == Pair(2, Pair(4, Pair(6, End()))))
    assert(list.map(_ + 1) == Pair(2, Pair(3, Pair(4, End()))))
    assert(list.map(_ / 3) == Pair(0, Pair(0, Pair(1, End()))))
  }

  val list = List(1, 2, 3)
  list.flatMap((x) => List(x, -x))

  import com.essential.ch5.Q5_4_4._

  val listMaybe = List(Full(3), Full(2), Full(1))
  assert(listMaybe.map(maybe => if (maybe.value % 2 != 0) Empty[Int]() else maybe) == List(Empty[Int], Full(2), Empty[Int]))


  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C =
      this match {
        case Failure(a) => left(a)
        case Success(b) => right(b)
      }

    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(f(b))
      }

    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
  }

  final case class Failure[A, B](value: A) extends Sum[A, B]

  final case class Success[A, B](value: B) extends Sum[A, B]

}

object Q5_6_4 {

  sealed trait Maybe[+A]

  final case class Full[A](value: A) extends Maybe[A]

  final case object Empty extends Maybe[Nothing]

}

object Q5_6_6 {

  sealed trait Sum[+A, +B] {
    def fold[C](left: A => C, right: B => C): C =
      this match {
        case Failure(a) => left(a)
        case Success(b) => right(b)
      }

    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(f(b))
      }

    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
  }

  final case class Failure[A, B](value: A) extends Sum[A, B]

  final case class Success[A, B](value: B) extends Sum[A, B]

  sealed trait Expression {
    def eval: Sum[String, Double] =
      this match {
        case Addition(l, r) => lift2(l,r,(d1,d2)=>Success(d1+d2))
        case Subtraction(l, r) => lift2(l,r,(d1,d2)=>Success(d1-d2))
        case Division(l,r) => lift2(l,r,(d1,d2)=>if (d2==0) Failure("Division by zero") else Success(d1/d2))
        case SquareRoot(value) => value.eval flatMap {
          dbl => if (dbl < 0) Failure("Square root of negative number") else Success(math.sqrt(dbl))
        }
        case Number(value) => Success(value)
      }

    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval flatMap { left =>
        r.eval flatMap { right =>
          f(left, right)
        }
      }
  }

  final case class Addition(left: Expression, right: Expression) extends Expression

  final case class Subtraction(left: Expression, right: Expression) extends Expression

  final case class Division(left: Expression, right: Expression) extends Expression

  final case class SquareRoot(value: Expression) extends Expression

  final case class Number(value: Double) extends Expression

  def main(args: Array[String]): Unit = {
    assert(Addition(Number(1), Number(2)).eval == Success(3))
    //    assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
    assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))
  }

}