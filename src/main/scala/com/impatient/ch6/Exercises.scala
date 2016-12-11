package com.impatient.ch6


object Exercises {
  def main(args: Array[String]): Unit = {
    println(InchesToCentimeters.convert(1))
    println(Suit.Heart)
    println(Point(1,2))
    println(suitRedColor(Suit.Heart))
  }
  def suitRedColor(suit:Suit.Value):Boolean = {
    if (suit == Suit.Diamond || suit == Suit.Heart) true
    else false
  }
}

object Reverse extends App {
  println(args.reverse.mkString(" "))
}

class UnitConversion(factor: Double) {
  def convert(input: Double): Double = input * factor
}

object InchesToCentimeters extends UnitConversion(2.54)

object GallonsToLiters extends UnitConversion(3.78541)

object MilesToKilometers extends UnitConversion(1.60934)

object Conversion {
  def inchesToCentimeters(inch: Double): Double = inch * 2.54

  def gallonsToLiters(gallons: Double): Double = gallons * 3.78541

  def milesToKilometers(miles: Double): Double = miles * 1.60934
}

class Point(val x: Int, val y: Int) {
  override def toString:String = s"Point($x,$y)"
}

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}

object Suit extends Enumeration {
  type Suit = Value
  val Club = Value("♣")
  val Heart = Value("♥")
  val Spade = Value("♠")
  val Diamond = Value("♦")
}

object RGB extends Enumeration {
  val Black = Value(0x000000)
  val Red = Value(0xff0000)
  val Green = Value(0x00ff00)
  val Yellow = Value(0xffff00)
  val Blue = Value(0x0000ff)
  val Cyan = Value(0x00ffff)
  val Magenta = Value(0xff00ff)
  val White = Value(0xffffff)
}