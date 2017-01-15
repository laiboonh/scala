package com.essential.ch7

object Q7_1_6 {

  final case class Rational(numerator: Int, denominator: Int)

  implicit val sortRational = Ordering.fromLessThan[Rational]((a, b) => ((a.numerator: Double) / a.denominator) < ((b.numerator: Double) / b.denominator))

  def main(args: Array[String]): Unit = {
    assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
      List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
  }
}

object Q7_2_5 {

  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  object Order {
    implicit val sortByUnits = Ordering.fromLessThan[Order]((ord1, ord2) => ord1.units < ord2.units)
  }

  object AnotherOrder {
    implicit val sortByTotalPrice = Ordering.fromLessThan[Order]((ord1, ord2) => ord1.totalPrice < ord2.totalPrice)
  }

  def main(args: Array[String]): Unit = {
    import AnotherOrder.sortByTotalPrice
    val orders = List(Order(100, 1), Order(3, 3), Order(2, 2))
    println(orders.sorted)
  }
}

object Q7_3_4 {

  trait Equal[A] {
    def equal(elem1: A, elem2: A): Boolean
  }

  case class Person(name: String, email: String)

  object EmailImplicit {

    implicit object EmailEqual extends Equal[Person] {
      override def equal(elem1: Person, elem2: Person): Boolean = elem1.email == elem2.email
    }

  }

  object NameEmailImplicit {

    implicit object NameEmailEqual extends Equal[Person] {
      override def equal(elem1: Person, elem2: Person): Boolean = elem1.email == elem2.email && elem1.name == elem2.name
    }

  }


  object Eq {
    def apply[A](elem1: A, elem2: A)(implicit comparator: Equal[A]): Boolean = comparator.equal(elem1, elem2)
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]) = instance
  }

  def main(args: Array[String]): Unit = {
    import EmailImplicit._
    println(Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")))
    println(Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")))
  }
}

object Q7_6_2 {

  object IntImplicits {

    implicit class IntOps(int: Int) {
      def yeah: Unit = {
        times(_ => println("Oh yeah!"))
      }

      def times(func: Int => Unit): Unit = {
        if (int > 0) (0 until int).foreach(func)
      }


    }

  }

  object Equal {

    implicit class ToEqual[A](in: A) {
      def ===(other: A)(implicit equal: Equal[A]): Boolean = {
        equal.equal(in, other)
      }
    }

    def apply[A](implicit instance: Equal[A]): Equal[A] =
      instance
  }


  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }


  def main(args: Array[String]): Unit = {
    import IntImplicits._
    (3).yeah
    //3.times(i => println(s"Look - it's the number $i!"))
    //    import Equal._
    //
    //    implicit val intEq = new Equal[Int] {
    //      override def equal(v1: Int, v2: Int): Boolean = v1==v2
    //    }
    //
    //    println(3 === 10)
    //
    //    implicit val strEq = new Equal[String] {
    //      override def equal(v1: String, v2: String): Boolean = v1.toLowerCase == v2.toLowerCase
    //    }
    //
    //    println("abc" === "ABC")

    //    implicit def intToIntOps(int:Int):IntOps = new IntOps(int)
    //    (3).yeah

  }
}

object Q7_9_1 {
  import java.util.Date

  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    def stringify = values
      .map { case (name, value) => "\"" + name + "\":" + value.stringify }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }

  val obj = JsObject(Map("foo" -> JsString("a"), "bar" -> JsString("b"), "baz" -> JsString("c")))

  trait JsWriter[T] {
    def write(value: T): JsValue
  }

  implicit class JsUtil[A](value:A) {
    def toJson(implicit writer: JsWriter[A]): JsValue = writer.write(value)
  }

  implicit object AnonymousWriter extends JsWriter[Anonymous] {
    override def write(value: Anonymous): JsValue = JsObject(Map("id"->JsString(value.id), "createdAt"->JsString(value.createdAt.toString)))
  }
  implicit object UserWriter extends JsWriter[User] {
    override def write(value: User): JsValue = JsObject(Map("id"->JsString(value.id), "email"->JsString(value.email), "createdAt"->JsString(value.createdAt.toString)))
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User      => user.toJson
    }
  }

  def main(args: Array[String]): Unit = {
    val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))
    visitors.foreach{
      visitor => println(
        (visitor).toJson.stringify
      )
    }

    println(Anonymous("001", new Date).toJson.stringify)
  }

  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime() - createdAt.getTime()
  }

  final case class Anonymous(
                              val id: String,
                              val createdAt: Date = new Date()
                            ) extends Visitor

  final case class User(
                         val id: String,
                         val email: String,
                         val createdAt: Date = new Date()
                       ) extends Visitor

}