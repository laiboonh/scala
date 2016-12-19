package com.impatient.ch18

object Q1 {

  class Bug {
    var position: Int = 0
    var forward: Boolean = true

    def show(): Bug = {
      print(s"$position ")
      this
    }

    def move(spaces: Int): Bug = {
      if (forward) {
        position += spaces
      } else {
        position -= spaces
      }
      this
    }

    def turn(spaces: Int): Bug = {
      forward = !forward
      move(spaces)
      this
    }
  }

  def main(args: Array[String]): Unit = {
    val bugsy = new Bug()
    bugsy.move(4).show().move(6).show().turn(5).show()
  }
}

object Q2 {

  class Bug {
    var position: Int = 0
    var forward: Boolean = true

    def show(): this.type = {
      print(s"$position ")
      this
    }

    def move(spaces: Int): this.type = {
      if (forward) {
        position += spaces
      } else {
        position -= spaces
      }
      this
    }

    def turn(spaces: Int): this.type = {
      forward = !forward
      move(spaces)
      this
    }

    def turn(): this.type = {
      forward = !forward
      this
    }
  }

  object Show

  object Then

  object Around

  val show = Show
  val then = Then
  val around = Around

  trait FluentBug {
    this: Bug =>
    def and(show: Show.type): this.type = this.show()

    def and(then: Then.type): this.type = this

    def turn(around: Around.type): this.type = this.turn()
  }

  def main(args: Array[String]): Unit = {
    val bugsy = new Bug() with FluentBug
    bugsy move 4 and show and then move 6 and show turn around move 5 and show
  }
}

object Q3 {

  object Title

  object Author

  class Document {
    private var title = ""
    private var author = ""
    private var useNextArgAs: Any = null

    def set(obj: Title.type): this.type = {
      useNextArgAs = obj
      this
    }

    def set(obj: Author.type): this.type = {
      useNextArgAs = obj
      this
    }

    def to(arg: String): this.type = if (useNextArgAs == Title) {
      title = arg
      this
    } else {
      author = arg
      this
    }

    override def toString: String = s"$title - $author"
  }

  def main(args: Array[String]): Unit = {
    val book = new Document()
    book set Title to "Scala for the impatient" set Author to "Cay Horstmann"
    println(book)
  }

}

object Q4 {

  import collection.mutable.ArrayBuffer

  class Network {
    outer =>

    class Member(val name: String) {
      val contacts = new ArrayBuffer[Member]

      private def getNetwork: Network = outer

      override def equals(obj: scala.Any): Boolean = obj match {
        case (obj: Member) => getNetwork.equals(obj.getNetwork)
        case _ => false
      }
    }

    private val members = new ArrayBuffer[Member]

    def join(name: String) = {
      val m = new Member(name)
      members += m
      m
    }
  }

  type NetworkMember = n.Member forSome {val n: Network}

  def process(m1: NetworkMember, m2: NetworkMember) = (m1, m2)

  def processA[T <: n.Member forSome {val n : Network}](m1: T, m2: T) = (m1, m2)

  def main(args: Array[String]): Unit = {
    val chatter = new Network
    val alice = chatter.join("Alice")
    val catherine = chatter.join("Catherine")
    val myFace = new Network
    val bob = myFace.join("Bob")

    println(alice equals catherine)
    processA(alice, catherine)


  }
}

object Q6 {
  def fun(ints: Array[Int], int: Int): Either[Int, Int] =
    ints.zipWithIndex.collectFirst {
      case (integer, index) if integer == int => Right(index)
      case (integer, index) if integer > int => Left(index)
    }.getOrElse(Right(ints.size - 1))

  def main(args: Array[String]): Unit = {
    println(fun(Array(1, 2, 3, 4, 5, 6), 2))
    println(fun(Array(1, 2, 3, 4, 5, 6), 10))
  }
}

object Q7 {

  class Closeable {
    def run(obj: {def close(): Unit}): Unit = {
      try {
        println("running...")
      } finally {
        obj.close()
      }
    }
  }

  class Door {
    def close(): Unit = {
      println("closing door")
    }
  }

  def main(args: Array[String]): Unit = {
    val door = new Door

    new Closeable().run(door)
  }

}


object Q8 {
    def printValues(f: {def apply(x: Int): Int}, from: Int, to: Int): Unit = {
      println(
        (from to to).map(f(_)).mkString(" ")
      )
    }

    def main(args: Array[String]): Unit = {
      val anonfun1 = new Function1[Int, Int] {
        final def apply(x: Int): Int = x * x
      }
      val fun1 = (x:Int)=>x*x
      printValues(anonfun1, 3, 6)
      printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55), 3, 6)
    }
}

object Q9 {
  abstract class Dim[T](val value:Double, val name:String) {
    this: T =>
    protected def create(v:Double):T
    def +(other:Dim[T]) = create(value+other.value)
    override def toString = value + " " + name
  }
  class Seconds(v:Double) extends Dim[Seconds](v,"s") {
    override def create(v:Double)= new Seconds(v)
  }
  class Meters(v:Double) extends Dim[Meters](v, "m") {
    override def create(v:Double) = new Meters(v)
  }
}

object Q10 {

}