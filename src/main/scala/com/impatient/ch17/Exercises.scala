package com.impatient.ch17

object Q1 {

  class Pair[T, S](first: T, second: S) {
    def swap = new Pair(second, first)

    override def toString: String = s"$first, $second"
  }

  def main(args: Array[String]): Unit = {
    println(new Pair("hi", "bob").swap)
  }
}

object Q2 {

  class Pair[T](var first: T, var second: T) {
    def swap = {
      val firstVal = first
      first = second
      second = firstVal
    }

    override def toString: String = s"$first, $second"
  }

  def main(args: Array[String]): Unit = {
    val p = new Pair(2, 1)
    p.swap
    println(p)
  }
}

object Q3 {

  class Pair[T, S](val first: T, val second: S) {
    override def toString: String = s"$first, $second"
  }

  def swap[T,S](input: Pair[T, S]) = new Pair(input.second, input.first)

  def main(args: Array[String]): Unit = {
    println(swap(new Pair(2, 1)))
  }
}

object Q4 {
  class Person
  class Student extends Person

  class Pair[T](var first: T, var second: T) {
    def replaceFirst(newFirst:T) = new Pair[T](newFirst,second)
    override def toString: String = s"$first, $second"
  }
  def main(args: Array[String]): Unit = {
    val pair:Pair[Person] = new Pair(new Person, new Person)
    pair.replaceFirst(new Student)
  }

}

object Q6 {
  def middle[T](elems:Iterable[T]):T = {
    elems.drop(elems.size/2).head
  }
  def main(args: Array[String]): Unit = {
    println(middle("hello"))
  }
}

object Q7 {
  class Person(val name: String)
  class Student(name: String) extends Person(name)

  val alice = new Person("Alice")
  val bob = new Person("Bob")
  val catherine = new Student("Catherine")

  val itP = Iterable[Person](alice, bob)
  val itS = Iterable[Student](catherine)

  def count[T](iterable:Iterable[T]) = iterable.size

  count(itP)
  count(itS)
  //you can use covariant type parameter in the result type but not parameter type
}

object Q10 {
  class Pair[T,S](first:T,second:S) {
    def swap(implicit ev : T =:= S) = new Pair(second, first)
    override def toString: String = s"$first, $second"
  }
  def main(args: Array[String]): Unit = {
    println(new Pair(1, 2).swap)
  }
}