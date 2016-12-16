package com.impatient.ch13

import java.io.File

import scala.collection.immutable.{HashMap, SortedMap, SortedSet}
import scala.io.Source

object Q1 {
  def main(arg: Array[String]): Unit = {
    val res = "Mississippi".foldRight(SortedMap[Char, Int]())((char, acc) => {
      acc + (char -> (acc.getOrElse(char, 0) + 1))
    })

    val res1 = "Mississippi".zipWithIndex.foldRight(SortedMap[Char, SortedSet[Int]]())((tup, acc) => {
      if (acc.contains(tup._1)) acc + (tup._1 -> (acc(tup._1) + tup._2))
      else acc + (tup._1 -> SortedSet(tup._2))
    })
    println(res)
  }
}

object Q4 {
  val names = Array("Tom", "Fred", "Harry")
  val attendence = Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)

  def fun(names: Array[String], attendence: Map[String, Int]): Array[Int] = {
    names.flatMap(name => attendence.get(name))
  }

  def main(arg: Array[String]): Unit = {
    println(fun(names, attendence).mkString(","))
  }
}

object Q5 {
  def mkString[A](arr: Array[A]): String = {
    arr.map(_.toString).reduceLeft((acc, a) => acc + " " + a)
  }

  def main(arg: Array[String]): Unit = {
    println(mkString(Array(1, 2, 3, 4, 5)))
  }
}

object Q6 {
  def main(arg: Array[String]): Unit = {
    val lst = List(1, 2, 3)
    println((List[Int]() /: lst) ((acc, num) => num :: acc))
  }
}

object Q7 {
  val res = (Array(1, 2, 3).zip(Array(4, 5, 6))).map(((a: Int, b: Int) => a * b).tupled)

  def main(arg: Array[String]): Unit = {
    println(res.mkString(","))
  }
}

object Q8 {
  def fun(arr: Array[Int], numCol: Int): Array[Array[Int]] =
    arr.grouped(numCol).foldRight(Array[Array[Int]]())((elem, acc) => elem +: acc)


  def main(arg: Array[String]): Unit = {
    (fun(Array(1, 2, 3, 4, 5, 6), 4)).foreach(x => println(x.mkString(",")))
  }
}

object Q10 {
  val f = new File("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/lorem.txt")
  val contents = Source.fromFile(f).mkString

  def main(arg: Array[String]): Unit = {

    def overwrite(a: HashMap[Char, Int], b: HashMap[Char, Int]): HashMap[Char, Int] = {
      b.foldRight(a)((pair, acc) => acc + pair)
    }

    val res = contents.par.aggregate(HashMap[Char, Int]())(
      (x, c) => x + (c -> (x.getOrElse(c, 0) + 1)),
      (map1, map2) => overwrite(map1, map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) })
    )

    println(res)
  }
}
