package com.impatient.ch8

abstract class Item {
  def price: Double

  def description: String
}

class SimpleItem(val price: Double, val description: String) extends Item {
  override def toString: String = s"$description : $price"
}

class Bundle(val items: List[Item]) extends Item {
  def price: Double = items.foldRight(0.0)((item, acc) => acc + item.price)

  def description: String = items.toString()
}

object itemUser {
  def main(args: Array[String]): Unit = {
    val apple = new SimpleItem(1.5, "apple")
    val orange = new SimpleItem(0.9, "orange")
    val bundle = new Bundle(List(apple,orange))
    println(s"${bundle.description} ${bundle.price}")
  }
}

class Point(val x:Int,val y:Int)

class LabeledPoint(label:String,x:Int,y:Int) extends Point(x,y)

abstract class Shape {
  def centerPoint:Point
}
class Circle(val centerPoint: Point, radius:Double) extends Shape
class Rectangle(val centerPoint:Point, length:Double, width:Double) extends Shape
class Square(cornerPoint:Point, width:Int) extends java.awt.Rectangle(cornerPoint.x, cornerPoint.y, width, width) {
  def this(width:Int) {
    this(new Point(0,0), width)
  }
  def this() {
    this(0)
  }
}


object shapeUser {
  def main(args: Array[String]): Unit = {
    new LabeledPoint("A",1,2)
  }
}


class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature {
  override def range = 2
}

object creatureUser {
  def main(args: Array[String]): Unit = {
    val ant = new Ant()
    println(ant.env.length)
  }
}