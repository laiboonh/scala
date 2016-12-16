package com.impatient.ch12

import scala.collection.GenSeq

object Q1 {
  def values(fun: (Int) => Int, low: Int, high: Int): Seq[(Int, Int)] = {
    (low to high).map(num => {
      (num, fun(num))
    })
  }

  def main(args: Array[String]): Unit = {
    println(values(x => x * x, -5, 5))
  }
}

object Q2 {
  def main(args: Array[String]): Unit = {
    val x: Int = Array[Int](-1, -2, -4, -2, -6, -9, -3).reduceLeft((acc: Int, num: Int) => if (num > acc) num else acc)
    println(x)
  }
}

object Q3 {
  def factorial(n: Int): Int = n match {
    case n if n > 0 => (1 to n).reduceLeft((acc: Int, num: Int) => num * acc)
    case _ => 0
  }

  def main(args: Array[String]): Unit = {
    println(factorial(-3))
  }
}

object Q4 {
  def factorial(n: Int): Int = (1 to n).foldLeft(1)((acc: Int, num: Int) => num * acc)

  def main(args: Array[String]): Unit = {
    println(factorial(-3))
  }
}

object Q5 {
  def largest(fun: Int => Int, inputs: Seq[Int]):Int = inputs.map(num=>fun(num)).max

  def main(args: Array[String]): Unit = {
    println(largest(x => 10 * x - x * x, 1 to 10))
  }
}

object Q6 {
  def largest(fun: Int => Int, inputs: Seq[Int]):Int =
    inputs.map(num=>(num,fun(num))).reduceLeft((acc,y)=> if(y._2 > acc._2) y else acc)._1

  def main(args: Array[String]): Unit = {
    println(largest(x => 10 * x - x * x, 1 to 10))
  }
}

object Q7 {
  def adjustToPair(fun:(Int,Int)=>Int) =
    (input:(Int,Int)) => fun(input._1, input._2)

  def main(args: Array[String]): Unit = {
    println(adjustToPair(_ * _)((6,7)))
  }
}

object Q8 {
  def main(args: Array[String]): Unit = {
    println(Array("hello", "world").corresponds(Array(5, 5))(_.length == _))
  }
}

object Q9 {
  def main(args: Array[String]): Unit = {
    def corresponds[A,B](arr1:Array[A], arr2:Array[B])(pred:(A,B)=>Boolean) =
      arr1.zip(arr2).forall((tup:(A,B))=>pred(tup._1,tup._2))
    println(corresponds(Array("hello", "world"), Array(5, 5))(_.length == _))
  }
}

object Q10 {
  def unless(condition: => Boolean)(block: => Unit) { if (!condition) { block } }

  def main(args: Array[String]): Unit = {
    unless (0 > 1) {
      println("Unless!")
    }
  }
}