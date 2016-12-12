package com.impatient.ch9

import java.io._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import scala.io.Source

object Q1 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/lorem.txt")
    val out = new PrintWriter("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/lorem-rev.txt")
    val lines: Iterator[String] = source.getLines()
    val newLines: String = lines.foldRight("")((elem, acc) => {
      out.println(elem);
      acc
    })
    out.close()
  }
}

object Q2 {
  def main(args: Array[String]): Unit = {
    val column = 8
    var count: Int = 0

    val source = Source.fromFile("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch9/02.txt")
    val out = new PrintWriter("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch9/02.out")

    for (c <- source) c match {
      case '\t' => {
        out.print(" " * (column - count % column))
        count = 0
      }
      case '\n' => {
        out.print(c)
        count = 0
      }
      case _ => {
        out.print(c)
        count += 1
      }
    }

    source.close()
    out.close()
  }
}

object Q3 {
  val path = "/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/lorem.txt"

  def main(args: Array[String]): Unit = {
    Source.fromFile(path).mkString.split("\\s+").filter(_.length > 12).foreach(println)
  }
}

object Q4 {
  val path = "/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch9/04.txt"

  def main(args: Array[String]): Unit = {

    val functions = List(
      (numbers: Array[Double]) => s"Sum: ${numbers.sum}",
      (numbers: Array[Double]) => s"Max: ${numbers.max}",
      (numbers: Array[Double]) => s"Min: ${numbers.min}",
      (numbers: Array[Double]) => if (numbers.length == 0) s"Average: 0" else s"Average: ${numbers.sum / numbers.length}"
    )

    val numbers: Array[Double] = Source.fromFile(path).mkString.split("\\s+").map(_.toDouble)
    functions.map(function => function(numbers)).foreach(println)

  }
}

object Q5 {
  def main(args: Array[String]): Unit = {
    val path = "/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch9/05.txt"
    val out = new PrintWriter(path)
    val n = 80
    val maxLength = BigInt(2).pow(n).toString().length
    for (i <- 0 to n) {
      val fst: BigInt = BigInt(2).pow(i)
      val snd: Double = 1 / fst.toDouble
      println(s"%${maxLength}d    %f".format(fst, snd))
    }
    out.close()
  }
}

object Q7 {
  def main(args: Array[String]): Unit = {
    val path = "/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch9/04.txt"
    val content: String = Source.fromFile(path).mkString
    val pattern = """\d+\.\d+""".r
    pattern.findAllIn(content).foreach(println)
  }
}

object Q8 {
  def main(args: Array[String]): Unit = {
    val content: String = Source.fromURL("http://horstmann.com/").mkString
    val pattern = """src=".+\.\w{3}"""".r
    pattern.findAllIn(content).foreach(println)
  }
}

object Q9 {
  def main(args: Array[String]): Unit = {
    val path: Path = FileSystems.getDefault().getPath("/Users/boonhui.lai/temp")
    var count = 0
    val visitor = new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (file.toString.endsWith(".class")) count = count + 1
        FileVisitResult.CONTINUE
      }
    }

    Files.walkFileTree(path, visitor)
    println(s"Count: $count")
  }
}

object Q10 {
  val path: String = "/Users/boonhui.lai/temp/person.obj"

  class Person(val name: String, val friends: List[Person]) extends Serializable

  def main(args: Array[String]): Unit = {
    val out = new ObjectOutputStream(new FileOutputStream(path))
    val alice = new Person("Alice", List.empty[Person])
    val bob = new Person("Bob", List.empty[Person])
    val cat = new Person("Cat", List(alice, bob))
    out.writeObject(cat)

    val in = new ObjectInputStream(new FileInputStream(path))
    val person = in.readObject().asInstanceOf[Person]
    person.friends.foreach(p => println(p.name))
  }
}