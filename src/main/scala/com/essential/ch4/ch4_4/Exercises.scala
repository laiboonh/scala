package com.essential.ch4.ch4_4

sealed trait TrafficLight {
  def next: TrafficLight
}

object TrafficLight {
  def next(now: TrafficLight): TrafficLight = now match {
    case Red => Green
    case Green => Yellow
    case Yellow => Red
  }
}

final case object Red extends TrafficLight {
  val next = Yellow
}

final case object Yellow extends TrafficLight {
  val next = Green
}

final case object Green extends TrafficLight {
  val next = Red
}

sealed trait Calculation

final case class Success(result: Int) extends Calculation

final case class Failure(message: String) extends Calculation

object Calculator {
  def +(calculation: Calculation, int: Int): Calculation = calculation match {
    case f@Failure(_) => f
    case Success(res) => Success(res + int)
  }

  def -(calculation: Calculation, int: Int): Calculation = calculation match {
    case f@Failure(_) => f
    case Success(res) => Success(res - int)
  }

  def /(calculation: Calculation, int: Int) = calculation match {
    case f@Failure(_) => f
    case Success(res) if (int == 0) => Failure("Division by zero")
    case Success(res) if (int != 0) => Success(res / int)
  }

  def main(args: Array[String]): Unit = {
    assert(Calculator.+(Success(1), 1) == Success(2))
    assert(Calculator.-(Success(1), 1) == Success(0))
    assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))
    assert(Calculator./(Success(4), 2) == Success(2))
    assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
    assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
  }
}

sealed trait Source

final case object well extends Source

final case object spring extends Source

final case object tap extends Source

sealed trait Water

final case class Bottled(size: Int, source: Source, carbonated: Boolean) extends Water