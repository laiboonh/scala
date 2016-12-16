package com.impatient.ch11

import java.io.File

import scala.collection.mutable.ArrayBuffer

object Q3 {

  class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    override def toString = num + "/" + den

    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

    def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

    def +(other: Fraction) = new Fraction(this.num * other.den + other.num * this.den, this.den * other.den)
  }

  def main(args: Array[String]): Unit = {
    println(new Fraction(1, 2) + new Fraction(1, 2))
  }

}

object Q4 {

  class Money(d: Int, c: Int) {
    private val cents: Int = c
    private val dollars: Int = d

    def +(other: Money) = {
      val dollarCarry = (cents + other.cents) / 100
      new Money(dollars + other.dollars, cents % other.cents)
    }

    override def toString = "$" + s"$dollars.$cents"

  }

  object Money {
    def apply(d: Int, c: Int) = new Money(d, c)
  }

  def main(args: Array[String]): Unit = {
    println(Money(1, 20) + Money(1, 90))
  }
}

object Q5 {

  class Table {
    var contents = ""

    def |(content: String): Table = {
      if (contents == "") contents = contents + s"<tr><td>$content</td>"
      else contents = contents + s"<td>$content</td>"
      this
    }

    def ||(content: String): Table = {
      contents = contents + s"</tr><td>$content</td>"
      this
    }

    override def toString = s"<table>${contents.toString}</tr></table>"
  }

  object Table {
    def apply() = new Table()
  }

  def main(args: Array[String]): Unit = {
    val t = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM,.NET"
    println(t)
  }

}

object Q6 {

  class ASCIIArt(val art: String) {

    def +(other: ASCIIArt) = new ASCIIArt(
      art.split("\n").zip(other.art.split("\n")).map(x => x._1 + x._2).mkString("\n")
    )

    def ^(other: ASCIIArt) = new ASCIIArt(
      art + "\n" + other.art
    )

    override def toString = art
  }


  val x = new ASCIIArt(
    """ /\_/\
( ' ' )
(  -  )
 | | |
(__|__)""")

  val y = new ASCIIArt(
    """   -----
 / Hello \
<  Scala |
 \ Coder /
   -----""")

  def main(args: Array[String]): Unit = {
    println(x + y)
    println(x ^ y)
  }
}

object Q7 {

  class BitSequence(private var value: Long = 0) {
    implicit def bool2int(b: Boolean) = if (b) 1 else 0

    def update(bit: Int, state: Int) = value |= (state & 1L) << bit % 64

    def apply(bit: Int): Int = if ((value & 1L << bit % 64) > 0) 1 else 0

    override def toString = "%64s".format(value.toBinaryString).replace(" ", "0")
  }

  def main(args: Array[String]): Unit = {
    val b = new BitSequence()
    b.update(10, 1)
    println(b)
  }

}

object Q8 {

  class Matrix(val mRows: Int, val nColumns: Int) {
    val contents = new Array[Array[Int]](_length = mRows).map(_ => new Array[Int](_length = nColumns))

    def setValue(m: Int, n: Int, value: Int) = {
      contents(m)(n) = value
    }

    def mat(m: Int, n: Int): Int = contents(m)(n)

    def *(scalar: Int): Unit = contents.foreach(arr => {
      for (i <- 0 to (nColumns - 1)) arr(i) = arr(i) * scalar
    })

    def *(other: Matrix): Matrix = {
      assert(nColumns == other.mRows, s"Cannot multiply $size by ${other.size}")
      val result = new Matrix(mRows, other.nColumns)
      for (i <- 0 to (mRows - 1); j <- 0 to (other.nColumns - 1)) {
        result.setValue(i, j, multiply(getRow(i), other.getColumn(j)))
      }
      result
    }

    private def multiply(x: Array[Int], y: Array[Int]): Int = {
      assert(x.length == y.length, s"multiplying array of length ${x.length} with array of length ${y.length}")
      val result = new Array[Int](_length = x.length)
      (x.zip(y)).map(tup => tup._1 * tup._2).sum
    }

    private def getColumn(n: Int): Array[Int] = {
      assert(0 <= n && n <= nColumns - 1, s"getting column ${n + 1} from a $size")
      val col = new Array[Int](_length = mRows)
      for (i <- 0 to (mRows - 1)) {
        col(i) = mat(i, n)
      }
      col
    }

    private def getRow(m: Int): Array[Int] = {
      assert(0 <= m && m <= mRows - 1, s"getting row ${m + 1} from a $size")
      contents(m)
    }

    def size: String = s"$mRows by $nColumns matrix"

    override def toString: String = contents.map(arr => arr.mkString(",")).mkString("\n")

  }

  object Matrix {
    def apply(mRows: Int, nColumns: Int) = new Matrix(mRows, nColumns)
  }

  def main(args: Array[String]): Unit = {
    val x = Matrix(2, 3)
    x.setValue(0, 0, 1)
    x.setValue(0, 1, 2)
    x.setValue(0, 2, 3)
    x.setValue(1, 0, 4)
    x.setValue(1, 1, 5)
    x.setValue(1, 2, 6)

    val y = Matrix(3, 2)
    y.setValue(0, 0, 7)
    y.setValue(0, 1, 8)
    y.setValue(1, 0, 9)
    y.setValue(1, 1, 10)
    y.setValue(2, 0, 11)
    y.setValue(2, 1, 12)

    println(x * y)
  }

}

object Q9 {
  class RichFile(path:String) {

  }
  object RichFile {
    def apply(path:String) = new RichFile(path)
    def unapply(fullPath:String):Option[(String,String,String)] = {
      val (pathAndFileName, ext) = fullPath.splitAt(fullPath.lastIndexOf('.'))
      val (path, fileName) = pathAndFileName.splitAt(pathAndFileName.lastIndexOf('/'))
      Some(path,fileName.drop(1),ext.drop(1))
    }
  }
  def main(args: Array[String]): Unit = {
    val x = "/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch11/Exercises.scala" match {
      case RichFile(path,name,ext) => s"path : $path, name: $name, ext: $ext"
      case _ => None
    }
    println(x)
  }
}

object Q10 {
  class RichFile(path:String) {

  }
  object RichFile {
    def apply(path:String) = new RichFile(path)
    def unapplySeq(fullPath:String):Option[Seq[String]] = {
      val (pathAndFileName, ext) = fullPath.splitAt(fullPath.lastIndexOf('.'))
      val (path, fileName) = pathAndFileName.splitAt(pathAndFileName.lastIndexOf('/'))
      val x= Some((path.split("/").toSeq) :+ fileName.drop(1) :+ ext.drop(1))
      x
    }
  }
  def main(args: Array[String]): Unit = {
    val x = "home/boonhui.lai/Exercises.scala" match {
      case RichFile(homeDir, userName ,name,ext) => s"home dir : $homeDir, user name : $userName, name: $name, ext: $ext"
      case _ => None
    }
    println(x)
  }
}