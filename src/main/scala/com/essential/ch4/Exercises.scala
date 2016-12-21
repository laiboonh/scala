package com.essential.ch4

trait Feline {
  def color: String

  def sound: String
}

case class Cat(color: String, sound: String = "meow", food: String) extends Feline

case class Lion(color: String, sound: String = "roar", maneSize: Int) extends Feline

case class Tiger(color: String, sound: String = "roar") extends Feline

case class Panther(color: String, sound: String = "roar") extends Feline

sealed trait Shape {
  def sides: Int

  def perimeter: Double

  def area: Double

  def color: Color
}

final case class Circle(radius: Double, color:Color) extends Shape {
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * radius * radius
}


sealed trait Rectangular extends Shape {
  val sides = 4

  def length: Double

  def width: Double

  val perimeter = 2 * length + 2 * width
  val area = length * width
}

final case class Rectangle(length: Double, width: Double, color:Color) extends Rectangular

final case class Square(length: Double, color:Color) extends Rectangular {
  val width = length
}

object Draw {
  def apply(shape: Shape): String = shape match {
    case Circle(radius, color) => s"A ${colorName(color)} circle of raduis $radius cm"
    case Rectangle(length, width, color) => s"A ${colorName(color)} rectangle of width $width cm and length $length cm"
    case Square(length, color) => s"A ${colorName(color)}   square of length $length cm"
  }

  def colorName(color:Color) = color match {
    case Yellow => "yellow"
    case Red => "red"
    case Pink => "pink"
    case colors:Colors => colors.lightOrDark
  }

  def main(args: Array[String]): Unit = {
    println(apply(Circle(10.0, Yellow)))
  }
}

sealed trait Color {
  def red: Double

  def green: Double

  def blue: Double
}

final case object Red extends Color {
  val red = 1.0
  val green = 0
  val blue = 0
}

final case object Yellow extends Color {
  val red = 1.0
  val green = 1.0
  val blue = 0
}

final case object Pink extends Color {
  val red = 1.0
  val green = 0
  val blue = 1.0
}

final case class Colors(red: Double, green: Double, blue: Double) extends Color {
  def lightOrDark:String = if(red+green+blue/3 > 0.5) "light" else "dark"
}

sealed trait DivisionResult
final case class Finite(result:Int) extends DivisionResult
final case object Infinite extends DivisionResult

object divide {
  def apply(num:Int,den:Int):DivisionResult = den match {
    case 0 => Infinite
    case d => Finite(num/d)
  }

  def main(args:Array[String]): Unit = {
    println(divide(1,2))
    println(divide(1,0))
  }
}